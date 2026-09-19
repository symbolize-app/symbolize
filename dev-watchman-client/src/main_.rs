use crate::nix_child::NixChild as _;
use anyhow::Result;
use anyhow::anyhow;
use backon::Retryable as _;
use clap;
use clap::Parser as _;
use nix::sys::signal::Signal;
use std::ffi::OsString;
use std::future::ready;
use std::path::Path;
use std::path::PathBuf;
use std::process::ExitCode;
use std::process::ExitStatus;
use std::sync::Arc;
use std::time::Duration;
use tokio::fs;
use tokio::process::Child;
use tokio::process::Command;
use tokio::select;
use tokio::sync::Semaphore;
use tokio::sync::TryAcquireError;
use tokio::task::JoinSet;
use watchman_client::CanonicalPath;
use watchman_client::Connector;
use watchman_client::SubscriptionData;
use watchman_client::expr::Expr;
use watchman_client::expr::NameTerm;
use watchman_client::fields::NameOnly;
use watchman_client::pdu::FileType;
use watchman_client::pdu::SubscribeRequest;

#[derive(Clone, Debug, clap::Parser)]
#[command(version)]
struct Cli {
  #[arg(
    short,
    long,
    conflicts_with = "mode",
    required_unless_present = "mode"
  )]
  target: Option<String>,

  #[arg(
    short,
    long,
    conflicts_with = "target",
    required_unless_present = "target"
  )]
  mode: Option<Mode>,

  #[arg(long, default_value = "debug")]
  modifier: String,

  #[arg(short, long)]
  restart: bool,

  #[arg(last = true)]
  command: Vec<String>,
}

#[derive(Clone, Debug)]
struct ResolvedTarget {
  executable_path: PathBuf,
  directory: PathBuf,
  file_name: OsString,
}

impl ResolvedTarget {
  async fn resolve(target: &str, modifier: &str) -> Result<Self> {
    let output = Command::new("buck2")
      .args(["build", "-m", modifier, "--show-simple-output", target])
      .output()
      .await
      .map_err(|e| {
        anyhow!("Failed to run buck2 build for {target}: {e}")
      })?;

    if !output.status.success() {
      let stderr = String::from_utf8_lossy(&output.stderr);
      return Err(anyhow!("buck2 build failed for {target}: {stderr}"));
    }

    let stdout = String::from_utf8(output.stdout).map_err(|e| {
      anyhow!("Invalid utf-8 in buck2 output for target {target}: {e}")
    })?;
    let rel_path = stdout.trim();
    if rel_path.is_empty() {
      return Err(anyhow!("Empty output path for target {target}"));
    }

    let executable_path =
      std::fs::canonicalize(rel_path).map_err(|e| {
        anyhow!(
          "Failed to canonicalize '{rel_path}' for target {target}: {e}"
        )
      })?;

    let directory = executable_path
      .parent()
      .ok_or_else(|| {
        anyhow!("missing parent for {}", executable_path.display())
      })?
      .to_path_buf();

    let file_name = executable_path
      .file_name()
      .ok_or_else(|| {
        anyhow!("missing file name for {}", executable_path.display())
      })?
      .to_os_string();

    Ok(Self {
      executable_path,
      directory,
      file_name,
    })
  }
}

#[derive(
  Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum,
)]
enum Mode {
  Gleam,
  Haskell,
  Rust,
  #[value(alias = "javascript")]
  JavaScript,
}

#[allow(clippy::print_stderr)]
#[tokio::main]
pub async fn main() -> Result<ExitCode> {
  match Cli::try_parse() {
    Ok(cli) => {
      let resolved_target = if let Some(target) = &cli.target {
        Some(ResolvedTarget::resolve(target, &cli.modifier).await?)
      } else {
        if cli.command.is_empty() {
          return Err(anyhow!(
            "command is required when --mode is specified"
          ));
        }
        None
      };

      let files_changed = Arc::new(Semaphore::new(0));
      let mut join_set = JoinSet::new();
      join_set.spawn(watch_files(
        cli.clone(),
        resolved_target.clone(),
        files_changed.clone(),
      ));
      join_set.spawn(run_command(cli, resolved_target, files_changed));
      let err = join_set
        .join_next()
        .await
        .ok_or(anyhow!("empty join set"))?
        .map_err(Into::into)
        .and_then(|inner_result| inner_result)
        .expect_err("join error");
      eprintln!("[watch] {err:?}");
      join_set.shutdown().await;
    }
    Err(err) => {
      err.print()?;
    }
  }
  Ok(ExitCode::from(1))
}

#[allow(clippy::print_stdout)]
async fn watch_files(
  cli: Cli,
  resolved_target: Option<ResolvedTarget>,
  files_changed: Arc<Semaphore>,
) -> Result<!> {
  let connector = Connector::new()
    .unix_domain_socket("build/watchman-unix-listener")
    .watchman_cli_path("/dev/null");
  let client = { || connector.connect() }
    .retry(
      backon::ExponentialBuilder::default()
        .with_jitter()
        .with_min_delay(Duration::from_millis(100))
        .with_max_delay(Duration::from_secs(2))
        .with_max_times(10),
    )
    .await?;
  println!("[watch] Connected to Watchman server");
  let root_path = if let Some(resolved) = &resolved_target {
    write_executable_config(&resolved.directory).await?;
    resolved.directory.as_path()
  } else {
    Path::new(".")
  };
  let root = client
    .resolve_root(CanonicalPath::canonicalize(root_path)?)
    .await?;
  println!(
    "[watch] Resolved root: {} ({})",
    root.project_root().display(),
    root.watcher()
  );
  let (mut subscription, _) = client
    .subscribe::<NameOnly>(
      &root,
      SubscribeRequest {
        since: None,
        relative_root: Some(".".into()),
        expression: Some(build_expr(&cli, resolved_target.as_ref())?),
        fields: vec!["name", "type"],
        empty_on_fresh_instance: false,
        case_sensitive: true,
        defer_vcs: true,
        defer: Vec::default(),
        drop: Vec::default(),
      },
    )
    .await?;
  println!("[watch] Subscription ready");
  loop {
    match subscription.next().await? {
      SubscriptionData::Canceled => {
        return Err(anyhow!("watch cancelled"));
      }
      SubscriptionData::FilesChanged(query_result) => {
        if matches!(
          files_changed.try_acquire(),
          Err(TryAcquireError::Closed)
        ) {
          return Err(anyhow!("semaphore closed"));
        }
        files_changed.add_permits(1);
        println!(
          "[watch] Files changed: {}",
          query_result.files.unwrap_or_default().len()
        );
      }
      _ => {}
    }
  }
}

async fn write_executable_config(program_dir: &Path) -> Result<()> {
  fs::write(program_dir.join(".watchmanconfig.json"), "{}").await?;
  Ok(())
}

fn build_expr(
  cli: &Cli,
  resolved_target: Option<&ResolvedTarget>,
) -> Result<Expr> {
  let file_expr = if let Some(resolved) = resolved_target {
    vec![Expr::Name(NameTerm {
      paths: vec![Path::new(&resolved.file_name).into()],
      wholename: true,
    })]
  } else {
    match cli.mode.expect("mode or target required") {
      Mode::Haskell => vec![
        Expr::Suffix(vec!["hs".into()]),
        Expr::Name(NameTerm {
          paths: vec!["BUCK".into()],
          wholename: false,
        }),
        Expr::Name(NameTerm {
          paths: vec![
            "dev-hlint/base.yaml".into(),
            "dev-task/haskell.yml".into(),
          ],
          wholename: true,
        }),
      ],
      Mode::Rust => vec![
        Expr::Suffix(vec!["rs".into(), "sql".into()]),
        Expr::Name(NameTerm {
          paths: vec!["BUCK".into(), "workspace.bzl".into()],
          wholename: false,
        }),
        Expr::Name(NameTerm {
          paths: vec!["dev-task/rust.yml".into()],
          wholename: true,
        }),
      ],
      Mode::Gleam => vec![
        Expr::Suffix(vec!["gleam".into()]),
        Expr::Name(NameTerm {
          paths: vec!["gleam.toml".into(), "manifest.toml".into()],
          wholename: false,
        }),
        Expr::Name(NameTerm {
          paths: vec!["Taskfile.yml".into()],
          wholename: false,
        }),
      ],
      Mode::JavaScript => vec![
        Expr::Suffix(vec![
          "cjs".into(),
          "css".into(),
          "gleam".into(),
          "html".into(),
          "js".into(),
          "mjs".into(),
          "sql".into(),
          "toml".into(),
          "txt".into(),
        ]),
        Expr::Name(NameTerm {
          paths: vec![
            ".eslintrc.json".into(),
            "BUCK".into(),
            "vendor/README.md".into(),
          ],
          wholename: false,
        }),
        Expr::Name(NameTerm {
          paths: vec![
            "dev-buck/esbuild.bzl".into(),
            "dev-buck/gleam.bzl".into(),
            "dev-eslint/index.json".into(),
            "dev-task/node.yml".into(),
          ],
          wholename: true,
        }),
      ],
    }
  };
  Ok(Expr::All(vec![
    Expr::FileType(FileType::Regular),
    Expr::Any(file_expr),
  ]))
}

async fn run_command(
  cli: Cli,
  resolved_target: Option<ResolvedTarget>,
  files_changed: Arc<Semaphore>,
) -> Result<!> {
  let mut child: Option<Child> = None;
  loop {
    select! {
      permit = files_changed.acquire(),
      if cli.restart || child.is_none() => {
        permit?.forget();
        child = Some(handle_files_changed(&cli, resolved_target.as_ref(), child).await?);
      }
      status = async {
        child.as_mut().map(Child::wait).expect("some child").await
      }, if child.is_some() => {
        child = handle_child_done(
          &cli, resolved_target.as_ref(), status?
        ).await?;
      }
    }
  }
}

async fn handle_files_changed(
  cli: &Cli,
  resolved_target: Option<&ResolvedTarget>,
  child: Option<Child>,
) -> Result<Child> {
  match child {
    Some(child) => {
      restart_child(&child)?;
      Ok(child)
    }
    None => Ok(start_child(cli, resolved_target).await?),
  }
}

#[allow(clippy::print_stdout)]
async fn handle_child_done(
  cli: &Cli,
  resolved_target: Option<&ResolvedTarget>,
  status: ExitStatus,
) -> Result<Option<Child>> {
  if let Some(code) = status.code() {
    println!("[watch] Exit code {code}");
  } else {
    println!("[watch] Unknown exit code");
  }
  if cli.restart {
    Ok(Some(start_child(cli, resolved_target).await?))
  } else {
    Ok(None)
  }
}

#[allow(clippy::print_stdout)]
async fn start_child(
  cli: &Cli,
  resolved_target: Option<&ResolvedTarget>,
) -> Result<Child> {
  let (program, args): (PathBuf, &[String]) =
    if let Some(resolved) = resolved_target {
      (resolved.executable_path.clone(), &cli.command)
    } else {
      let prog = cli
        .command
        .first()
        .ok_or_else(|| anyhow!("empty command"))?;
      (PathBuf::from(prog), &cli.command[1 ..])
    };
  let program_display = program.display().to_string();
  let child = { || ready(Command::new(&program).args(args).spawn()) }
    .retry(
      // Retry in case an executable is still being written
      backon::ExponentialBuilder::default()
        .with_jitter()
        .with_min_delay(Duration::from_millis(10))
        .with_max_delay(Duration::from_millis(200))
        .with_max_times(10),
    )
    .await?;
  println!("[watch] Started {program_display}");
  Ok(child)
}

#[allow(clippy::print_stdout)]
fn restart_child(child: &Child) -> Result<()> {
  child.signal_kill(Signal::SIGTERM)?;
  println!("[watch] Restarting");
  Ok(())
}

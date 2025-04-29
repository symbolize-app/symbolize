use anyhow::Result;
use anyhow::anyhow;
use clap;
use clap::Parser as _;
use serde::Deserialize;
use serde_json;
use std::collections::HashSet;
use std::path::PathBuf;
use std::process::ExitCode;
use std::process::Stdio;
use tokio::fs;
use tokio::io::AsyncBufReadExt as _;
use tokio::io::BufReader;
use tokio::process::Child;
use tokio::process::Command;

#[derive(Clone, Debug, clap::Parser)]
#[command(version)]
struct Cli {
  #[arg(short, long)]
  mode: Mode,

  #[arg(short, long)]
  clippy: Option<String>,
}

#[derive(
  Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum,
)]
enum Mode {
  Debug,
  Release,
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
enum AnyEvent {
  Specific(Event),
  Unknown {},
}

#[derive(Deserialize, Debug)]
#[serde(tag = "reason")]
enum Event {
  #[serde(rename = "compiler-artifact")]
  CompilerArtifact {
    target: EventTarget,
    profile: EventProfile,
    executable: PathBuf,
    fresh: bool,
  },
  #[serde(rename = "compiler-message")]
  CompilerMessage {
    target: EventTarget,
    message: EventMessage,
  },
}

#[derive(Deserialize, Debug)]
struct EventTarget {
  name: String,
}

#[derive(Deserialize, Debug)]
struct EventProfile {
  test: bool,
}

#[derive(Deserialize, Debug)]
struct EventMessage {
  rendered: String,
}

#[allow(clippy::print_stdout)]
#[allow(clippy::print_stderr)]
#[tokio::main]
pub async fn main() -> Result<ExitCode> {
  match Cli::try_parse() {
    Ok(cli) => {
      let mut child = Command::new("cargo")
        .args([
          "build",
          "--all-targets",
          "--quiet",
          "--message-format=json-diagnostic-rendered-ansi",
        ])
        .args(
          (cli.mode == Mode::Release)
            .then_some(vec!["--locked", "--release"])
            .unwrap_or_default(),
        )
        .stdout(Stdio::piped())
        .spawn()?;
      let events_error = process_events(&mut child)
        .await
        .inspect_err(|err| {
          eprintln!("Build event error: {err:?}");
        })
        .is_err();
      let exit_error = child.wait().await?.exit_ok().is_err();
      let mut error = events_error || exit_error;
      if !error && let Some(clippy) = cli.clippy {
        error = Command::new("cargo")
          .args(["clippy", "--all-targets", "--quiet", "--offline"])
          .args(
            (cli.mode == Mode::Release)
              .then_some(vec!["--locked", "--release"])
              .unwrap_or_default(),
          )
          .args(clippy.split(' '))
          .status()
          .await?
          .exit_ok()
          .is_err();
      }
      Ok(ExitCode::from(u8::from(error)))
    }
    Err(err) => {
      err.print()?;
      Ok(ExitCode::from(1))
    }
  }
}

#[allow(clippy::print_stdout)]
async fn process_events(child: &mut Child) -> Result<()> {
  let stdout = child.stdout.take().ok_or(anyhow!("missing stdout"))?;
  let mut seen_messages = HashSet::new();
  let reader = BufReader::new(stdout);
  let mut lines = reader.lines();
  while let Some(line) = lines.next_line().await? {
    let event: AnyEvent = serde_json::from_str(line.as_ref())?;
    match event {
      AnyEvent::Specific(Event::CompilerArtifact {
        target,
        profile,
        executable,
        fresh,
      }) if target.name.starts_with("symbolize")
        && profile.test
        && !fresh =>
      {
        let destination = executable
          .parent()
          .ok_or(anyhow!("missing parent"))?
          .parent()
          .ok_or(anyhow!("missing parent"))?
          .join(format!("{}-test", target.name.replace('_', "-")));
        fs::copy(executable, destination).await?;
      }
      AnyEvent::Specific(Event::CompilerMessage { target, message }) => {
        let pair = (target.name, message.rendered);
        if !seen_messages.contains(&pair) {
          print!("{}", pair.1);
          seen_messages.insert(pair);
        }
      }
      _ => {}
    }
  }
  Ok(())
}

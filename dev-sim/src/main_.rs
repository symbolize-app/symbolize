use anyhow::anyhow;
use anyhow::Result;
use rustyline_async::Readline;
use rustyline_async::ReadlineEvent;
use rustyline_async::SharedWriter;
use std::io::Write as _;

#[allow(clippy::print_stdout)]
#[allow(clippy::print_stderr)]
#[tokio::main]
pub async fn main() -> Result<()> {
  let (mut readline, mut stdout) = Readline::new("sim > ".to_owned())?;

  let mut ctx = Context {
    stdout: stdout.clone(),
  };

  loop {
    let command = readline.readline().await?;
    match command {
      ReadlineEvent::Line(line) => {
        let line = line.trim();
        if !line.is_empty() {
          readline.add_history_entry(line.to_owned());
          for line in line.split(';') {
            let line = line.trim();
            if !line.is_empty()
              && run_command(&mut ctx, line)? == CommandResult::Quit
            {
              break;
            }
          }
        }
      }
      ReadlineEvent::Eof => {
        break;
      }
      ReadlineEvent::Interrupted => {
        writeln!(stdout, "^C")?;
      }
    }
  }

  readline.flush()?;
  Ok(())
}

#[derive(Clone)]
struct Context {
  stdout: SharedWriter,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum CommandResult {
  Continue,
  Quit,
}

#[derive(Clone, Debug)]
struct Command {
  name: &'static str,
  aliases: &'static [&'static str],
  run: fn(ctx: &mut Context) -> Result<CommandResult>,
}

static COMMANDS: &[Command] = &[
  Command {
    name: "ping",
    aliases: &["p"],
    run: run_todo,
  },
  Command {
    name: "heartbeat",
    aliases: &["b"],
    run: run_todo,
  },
  Command {
    name: "help",
    aliases: &["h"],
    run: run_help,
  },
  Command {
    name: "quit",
    aliases: &["q"],
    run: run_quit,
  },
];

fn run_command(ctx: &mut Context, line: &str) -> Result<CommandResult> {
  let parts: Vec<&str> = line.split_whitespace().collect();
  let (name_part, _param_parts) = parts
    .split_first()
    .ok_or(anyhow!("line missing command part"))?;

  for command in COMMANDS {
    if name_part == &command.name || command.aliases.contains(name_part) {
      return (command.run)(ctx);
    }
  }
  writeln!(ctx.stdout, "Unknown command `{name_part}`")?;
  Ok(CommandResult::Continue)
}

fn run_todo(ctx: &mut Context) -> Result<CommandResult> {
  writeln!(ctx.stdout, "TODO")?;
  Ok(CommandResult::Continue)
}

fn run_help(ctx: &mut Context) -> Result<CommandResult> {
  writeln!(ctx.stdout, "Commands:")?;
  for command in COMMANDS {
    let name = command.name;
    let aliases = command.aliases.join(" / ");
    writeln!(ctx.stdout, "  {name} ({aliases})")?;
  }
  Ok(CommandResult::Continue)
}

#[allow(clippy::unnecessary_wraps)]
fn run_quit(_ctx: &mut Context) -> Result<CommandResult> {
  Ok(CommandResult::Quit)
}

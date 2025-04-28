use crate::command as svc_command;
use crate::context as svc_context;
use crate::run as svc_run;
use anyhow::Result;
use rustyline_async::Readline;
use rustyline_async::ReadlineEvent;
use std::io::Write as _;

#[allow(clippy::print_stdout)]
#[allow(clippy::print_stderr)]
#[tokio::main]
pub async fn main() -> Result<()> {
  let (mut readline, mut stdout) = Readline::new("sim > ".to_owned())?;

  let mut ctx = svc_context::Context {
    stdout: stdout.clone(),
    selected_connection_id: 0,
    connections: vec![],
  };

  loop {
    let event = readline.readline().await?;
    match event {
      ReadlineEvent::Line(line) => {
        let line = line.trim();
        if !line.is_empty() {
          readline.add_history_entry(line.to_owned());
          match svc_run::run_line(&mut ctx, line) {
            Ok(svc_command::CommandStatus::Continue) => {}
            Ok(svc_command::CommandStatus::Quit) => {
              break;
            }
            Err(err) => {
              writeln!(stdout, "Error: {err}")?;
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

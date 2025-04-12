use crate::command as svc_command;
use crate::context as svc_context;
use anyhow::Result;
use std::io::Write as _;

pub fn run_help(
  ctx: &mut svc_context::Context,
) -> Result<svc_command::CommandStatus> {
  writeln!(ctx.stdout, "Commands:")?;
  for command in svc_command::COMMANDS {
    let name = command.name;
    let aliases = command.aliases.join(" / ");
    writeln!(ctx.stdout, "  {name} ({aliases})")?;
  }
  Ok(svc_command::CommandStatus::Continue)
}

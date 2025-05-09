use crate::begin as svc_begin;
use crate::command as svc_command;
use crate::connect as svc_connect;
use crate::context as svc_context;
use crate::disconnect as svc_disconnect;
use crate::end as svc_end;
use crate::help as svc_help;
use crate::parse as svc_parse;
use crate::ping as svc_ping;
use anyhow::Result;
use anyhow::anyhow;

pub fn run_line(
  ctx: &mut svc_context::Context,
  line: &str,
) -> Result<svc_command::CommandStatus> {
  for line in line.split(';') {
    let phrase = line.trim();
    if !phrase.is_empty()
      && run_phrase(ctx, phrase)? == svc_command::CommandStatus::Quit
    {
      return Ok(svc_command::CommandStatus::Quit);
    }
  }
  Ok(svc_command::CommandStatus::Continue)
}

pub fn run_phrase(
  ctx: &mut svc_context::Context,
  phrase: &str,
) -> Result<svc_command::CommandStatus> {
  let parts: Vec<&str> = phrase.split_whitespace().collect();
  let (name_part, param_parts) = parts
    .split_first()
    .ok_or(anyhow!("line missing command part"))?;
  let (name, id) = svc_parse::parse_command_id(name_part)?;
  let parameters =
    svc_parse::parse_command_parameters(name, id, param_parts)?;
  run_command(ctx, parameters)
}

fn run_command(
  ctx: &mut svc_context::Context,
  parameters: svc_command::CommandParameters,
) -> Result<svc_command::CommandStatus> {
  match parameters {
    svc_command::CommandParameters::Begin {} => svc_begin::run(ctx),
    svc_command::CommandParameters::Connect {} => svc_connect::run(ctx),
    svc_command::CommandParameters::Disconnect {} => {
      svc_disconnect::run(ctx)
    }
    svc_command::CommandParameters::End {} => svc_end::run(ctx),
    svc_command::CommandParameters::Help {} => svc_help::run_help(ctx),
    svc_command::CommandParameters::Ping {} => svc_ping::run(ctx),
    svc_command::CommandParameters::Quit {} => Ok(run_quit()),
    svc_command::CommandParameters::Sleep {} => todo!(),
    svc_command::CommandParameters::Tempo {} => todo!(),
    svc_command::CommandParameters::Use {} => todo!(),
  }
}

fn run_quit() -> svc_command::CommandStatus {
  svc_command::CommandStatus::Quit
}

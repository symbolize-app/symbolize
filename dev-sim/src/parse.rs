use crate::command as svc_command;
use anyhow::Result;
use anyhow::anyhow;

pub fn parse_command_id(
  name_part: &str,
) -> Result<(&'static str, svc_command::CommandId)> {
  for command in svc_command::COMMANDS {
    if name_part == command.name || command.aliases.contains(&name_part) {
      return Ok((command.name, command.id));
    }
  }
  Err(anyhow!("unknown command `{name_part}`"))
}

pub fn parse_command_parameters(
  name: &'static str,
  id: svc_command::CommandId,
  param_parts: &[&str],
) -> Result<svc_command::CommandParameters> {
  match id {
    svc_command::CommandId::Begin => {
      parse_parts_0(name, param_parts)?;
      Ok(svc_command::CommandParameters::Begin {})
    }
    svc_command::CommandId::Connect => {
      parse_parts_0(name, param_parts)?;
      Ok(svc_command::CommandParameters::Connect {})
    }
    svc_command::CommandId::Disconnect => {
      parse_parts_0(name, param_parts)?;
      Ok(svc_command::CommandParameters::Disconnect {})
    }
    svc_command::CommandId::End => {
      parse_parts_0(name, param_parts)?;
      Ok(svc_command::CommandParameters::End {})
    }
    svc_command::CommandId::Help => {
      parse_parts_0(name, param_parts)?;
      Ok(svc_command::CommandParameters::Help {})
    }
    svc_command::CommandId::Ping => {
      parse_parts_0(name, param_parts)?;
      Ok(svc_command::CommandParameters::Ping {})
    }
    svc_command::CommandId::Sleep => todo!(),
    svc_command::CommandId::Tempo => {
      parse_parts_0(name, param_parts)?;
      Ok(svc_command::CommandParameters::Tempo {})
    }
    svc_command::CommandId::Quit => {
      parse_parts_0(name, param_parts)?;
      Ok(svc_command::CommandParameters::Quit {})
    }
    svc_command::CommandId::Use => todo!(),
  }
}

fn parse_parts_0(name: &'static str, param_parts: &[&str]) -> Result<()> {
  if param_parts.is_empty() {
    Ok(())
  } else {
    Err(anyhow!("too many parameters for `{name}` (expected 0)"))
  }
}

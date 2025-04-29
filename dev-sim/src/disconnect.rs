use crate::command as svc_command;
use crate::context as svc_context;
use anyhow::Result;
use anyhow::anyhow;
use std::io::Write as _;

pub fn run(
  ctx: &mut svc_context::Context,
) -> Result<svc_command::CommandStatus> {
  let id = ctx.selected_connection_id;
  let sim_connection = ctx
    .connections
    .get(id)
    .ok_or_else(|| anyhow!("no connections"))?;
  (if sim_connection.cancellation_token.is_cancelled() {
    Err(anyhow!("cannot disconnect again"))
  } else {
    Ok(())
  })?;
  writeln!(ctx.stdout, "[c{id}] Disconnecting...")?;
  sim_connection.cancellation_token.cancel();
  Ok(svc_command::CommandStatus::Continue)
}

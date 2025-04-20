use crate::command as svc_command;
use crate::context as svc_context;
use anyhow::anyhow;
use anyhow::Result;
use std::io::Write as _;

pub fn run(
  ctx: &mut svc_context::Context,
) -> Result<svc_command::CommandStatus> {
  let id = ctx.selected_connection_id;
  let sim_connection = ctx
    .connections
    .get(id)
    .ok_or_else(|| anyhow!("no connections"))?;
  sim_connection
    .sender_rx
    .borrow()
    .as_ref()
    .ok_or_else(|| anyhow!("not connected yet"))?
    .as_ref()
    .map_err(|err| anyhow!("{err}"))?;
  writeln!(ctx.stdout, "[c{id}] Disconnecting...")?;
  sim_connection
    .sender_tx
    .send(Some(Err(anyhow!("[c{id}] disconnecting"))))?;
  Ok(svc_command::CommandStatus::Continue)
}

use crate::command as svc_command;
use crate::context as svc_context;
use anyhow::Result;
use anyhow::anyhow;
use std::io::Write as _;

pub fn run(
  ctx: &mut svc_context::Context,
) -> Result<svc_command::CommandStatus> {
  let cid = ctx.selected_connection_id;
  let sim_connection = ctx
    .connections
    .get_mut(cid)
    .ok_or_else(|| anyhow!("no connections"))?;
  let id = sim_connection.selected_stream_id;
  let sim_stream = sim_connection
    .streams
    .get_mut(id)
    .ok_or_else(|| anyhow!("no streams"))?;

  (if sim_stream.cancellation_token.is_cancelled() {
    Err(anyhow!("cannot disconnect again"))
  } else {
    Ok(())
  })?;
  writeln!(ctx.stdout, "[c{cid}_s{id}] Ending...")?;
  sim_stream.cancellation_token.cancel();

  Ok(svc_command::CommandStatus::Continue)
}

use crate::command as svc_command;
use crate::context as svc_context;
use anyhow::Result;
use anyhow::anyhow;

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

  sim_stream.send_message_tx.try_send("ping".to_owned())?;

  Ok(svc_command::CommandStatus::Continue)
}

use anyhow::Result;
use anyhow::anyhow;
use nix::sys::signal;
use nix::sys::signal::Signal;
use nix::unistd::Pid;
use tokio::process::Child;

pub trait NixChild {
  fn signal_kill(&self, signal: Signal) -> Result<()>;
}

impl NixChild for Child {
  fn signal_kill(&self, signal: Signal) -> Result<()> {
    let pid =
      Pid::from_raw(self.id().ok_or(anyhow!("missing PID"))?.try_into()?);
    signal::kill(pid, signal)?;
    Ok(())
  }
}

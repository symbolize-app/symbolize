use anyhow::Result;

#[allow(clippy::print_stdout)]
#[allow(clippy::print_stderr)]
#[tokio::main]
pub async fn main() -> Result<()> {
  println!("Hello world");
  Ok(())
}

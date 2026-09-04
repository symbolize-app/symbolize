import dev_esbuild_db as db
import lib_time

pub type Mode {
  Development
  Production
}

pub type Dev {
  Dev(mode: Mode, outdir: String)
}

pub type Context {
  Context(dev: Dev, database: db.Database, time: lib_time.Context)
}

pub fn dev(mode: Mode, outdir: String) -> Dev {
  Dev(mode, outdir)
}

pub fn context(
  dev: Dev,
  database: db.Database,
  time: lib_time.Context,
) -> Context {
  Context(dev, database, time)
}

pub fn mode(dev: Dev) -> Mode {
  dev.mode
}

pub fn outdir(dev: Dev) -> String {
  dev.outdir
}

pub fn dev_context(context: Context) -> Dev {
  context.dev
}

pub fn database(context: Context) -> db.Database {
  context.database
}

pub fn time(context: Context) -> lib_time.Context {
  context.time
}

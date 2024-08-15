import type * as devDatabase from '@/db.ts'
import type * as time from '@intertwine/lib-time'

export type Context = DevContext & devDatabase.Context & time.Context

export interface DevContext {
  readonly dev: Dev
}

class Dev {
  constructor(
    readonly mode: Mode,
    readonly outdir: string,
  ) {}
}

export type { Dev }

export function dev(mode: Mode, outdir: string): Dev {
  return new Dev(mode, outdir)
}

export enum Mode {
  development = 'development',
  production = 'production',
}

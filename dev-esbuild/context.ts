import type * as devDatabase from '@/db.ts'
import type * as time from '@intertwine/lib-time'

export type Context = DevContext & devDatabase.Context & time.Context

export interface DevContext {
  readonly dev: Dev
}

export class Dev {
  constructor(
    readonly mode: Mode,
    readonly outdir: string,
  ) {}
}

export enum Mode {
  development = 'development',
  production = 'production',
}

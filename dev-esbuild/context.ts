import type * as devDatabase from '@/database.ts'
import type * as time from '@intertwine/lib-time'

export type Context = devDatabase.Context &
  time.Context & {
    readonly mode: Mode
    readonly outdir: string
  }

export enum Mode {
  development = 'development',
  production = 'production',
}

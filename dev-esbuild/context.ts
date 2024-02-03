import type * as devDatabase from '@/database.ts'
import type * as time from '@intertwine/lib-time'

export type Context = devDatabase.Context &
  time.Context & {
    mode: Mode
    outdir: string
  }

export enum Mode {
  development = 'development',
  production = 'production',
}

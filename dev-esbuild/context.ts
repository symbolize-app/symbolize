import type * as devDatabase from '@/database.ts'
import type * as time from '@intertwine/lib-time'

export type Context = time.Context &
  devDatabase.Context & {
    outdir: string
    mode: Mode
  }

export enum Mode {
  development = 'development',
  production = 'production',
}

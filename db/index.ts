import * as time from '@tiny/core/time.ts'
import type * as query from '@tiny/db/index.ts'
import chalk from 'chalk'
import pg from 'pg'
import pgConnectionString from 'pg-connection-string'
import type * as typeFest from 'type-fest'

declare const databaseApiReadSymbol: unique symbol

export type DatabaseApiRead = typeFest.Opaque<
  query.Database,
  typeof databaseApiReadSymbol
>

export type ReadContext = {
  databaseApiRead: DatabaseApiRead
}

declare const databaseApiWriteSymbol: unique symbol

export type DatabaseApiWrite = typeFest.Opaque<
  query.Database,
  typeof databaseApiWriteSymbol
>

export type WriteContext = {
  databaseApiWrite: DatabaseApiWrite
}

export function initContext(): ReadContext & WriteContext {
  return {
    databaseApiRead: initDatabase<DatabaseApiRead>(
      process.env.DATABASE_URL_API_READ as string
    ),
    databaseApiWrite: initDatabase<DatabaseApiWrite>(
      process.env.DATABASE_URL_API_WRITE as string
    ),
  }
}

function initDatabase<Database extends query.Database>(
  connectionString: string
): Database {
  return {
    pool: openPool(connectionString),
  } as Database
}

function openPool(
  connectionString: string
): Pick<pg.Pool, 'query'> {
  const max = 10
  const user = pgConnectionString.parse(connectionString)
    .user
  if (!user) {
    throw new Error('No DB user')
  }

  const pool = new pg.Pool({
    connectionString,
    connectionTimeoutMillis: time.interval({ seconds: 1 }),
    idleTimeoutMillis: time.interval({ seconds: 10 }),
    ['idle_in_transaction_session_timeout']: time.interval({
      seconds: 1,
    }),
    max,
    ['query_timeout']: time.interval({ seconds: 1 }),
    ['statement_timeout']: time.interval({ seconds: 1 }),
  })
  pool.on('error', console.error)
  pool.on('connect', () => {
    console.log(
      chalk.magenta(
        `[${user}] DB client connected (${pool.totalCount} / ${max})`
      )
    )
  })
  pool.on('remove', () => {
    console.log(
      chalk.magenta(
        `[${user}] DB client removed (${pool.totalCount} / ${max})`
      )
    )
  })
  return pool
}

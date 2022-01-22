import type * as tinyError from '@tiny/core/error.ts'
import * as tinyTime from '@tiny/core/time.ts'
import type * as tinyDbQuery from '@tiny/db/query.ts'
import chalk from 'chalk'
import pg from 'pg'
import pgConnectionString from 'pg-connection-string'

export const read = Symbol('DB read')

export type Read = typeof read

export type ReadContext = tinyDbQuery.Context<Read>

export const write = Symbol('DB write')

export type Write = typeof write

export type WriteContext = tinyDbQuery.Context<Write>

export const retryConfig: Omit<
  tinyError.RetryConfig,
  'onError'
> = {
  maxAttempts: 15,
  minDelayMs: tinyTime.interval({ milliseconds: 10 }),
  windowMs: tinyTime.interval({ seconds: 30 }),
}

export function initContext(): ReadContext & WriteContext {
  return {
    databases: {
      [read]: initDatabase<Read>(
        process.env.DATABASE_URL_API_READ as string
      ),
      [write]: initDatabase<Write>(
        process.env.DATABASE_URL_API_WRITE as string
      ),
    },
    databaseRetryConfig: retryConfig,
  }
}

function initDatabase<DatabaseId extends symbol>(
  connectionString: string
): tinyDbQuery.Database<DatabaseId> {
  const max = 10
  const user = pgConnectionString.parse(
    connectionString
  ).user
  if (!user) {
    throw new Error('No DB user')
  }

  const pool = new pg.Pool({
    connectionString,
    connectionTimeoutMillis: tinyTime.interval({
      seconds: 1,
    }),
    idleTimeoutMillis: tinyTime.interval({ seconds: 10 }),
    ['idle_in_transaction_session_timeout']:
      tinyTime.interval({
        seconds: 1,
      }),
    max,
    ['query_timeout']: tinyTime.interval({ seconds: 1 }),
    ['statement_timeout']: tinyTime.interval({
      seconds: 1,
    }),
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
  return {
    async query<
      Params extends tinyDbQuery.SupportedType[],
      Row extends Record<string, tinyDbQuery.SupportedType>,
      Result
    >(
      query: tinyDbQuery.Query<
        DatabaseId,
        Params,
        Row,
        Result
      >,
      ...params: Params
    ): Promise<Result> {
      const result = await pool.query<Row, Params>(
        {
          name: query.name,
          text: query.text,
        },
        params
      )
      return query.transform(result.rows)
    },
  }
}

import * as time from '@tiny/core/time.ts'
import type * as dbQuery from '@tiny/db/query.ts'
import chalk from 'chalk'
import pg from 'pg'
import pgConnectionString from 'pg-connection-string'

export const read = Symbol('DB read')

export type Read = typeof read

export type ReadContext = dbQuery.Context<Read>

export const write = Symbol('DB write')

export type Write = typeof write

export type WriteContext = dbQuery.Context<Write>

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
  }
}

function initDatabase<DatabaseId extends symbol>(
  connectionString: string
): dbQuery.Database<DatabaseId> {
  const max = 10
  const user = pgConnectionString.parse(
    connectionString
  ).user
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
  return {
    async query<
      Values extends dbQuery.SupportedType[],
      Row extends Record<string, dbQuery.SupportedType>,
      Transform
    >(
      query: dbQuery.Query<
        DatabaseId,
        Values,
        Row,
        Transform
      >,
      ...values: Values
    ): Promise<Transform> {
      const result = await pool.query<Row, Values>(
        {
          name: query.name,
          text: query.text,
        },
        values
      )
      return query.transform(result.rows)
    },
  }
}

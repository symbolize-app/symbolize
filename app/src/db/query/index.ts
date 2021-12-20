import * as time from '@tiny/core/time.ts'
import * as query from '@tiny/db/query.ts'
import chalk from 'chalk'
import pg from 'pg'
import pgConnectionString from 'pg-connection-string'

declare const readSymbol: unique symbol

export type Read = typeof readSymbol

export type ReadContext = {
  databaseApiRead: query.Database<Read>
}

declare const writeSymbol: unique symbol

export type Write = typeof writeSymbol

export type WriteContext = {
  databaseApiWrite: query.Database<Write>
}

export function initContext(): ReadContext & WriteContext {
  return {
    databaseApiRead: initDatabase<Read>(
      process.env.DATABASE_URL_API_READ as string
    ),
    databaseApiWrite: initDatabase<Write>(
      process.env.DATABASE_URL_API_WRITE as string
    ),
  }
}

function initDatabase<Id>(
  connectionString: string
): query.Database<Id> {
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
  return query.createDatabase({
    async query<
      Values extends query.SupportedType[],
      Row extends Record<string, query.SupportedType>,
      Transform
    >(
      query: query.Query<Id, Values, Row, Transform>,
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
  })
}

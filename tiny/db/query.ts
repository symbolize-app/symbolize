import type * as db from '@tiny/db/index.ts'
import type * as typeFest from 'type-fest'

const queryNameBase = 36

let globalQueryNameCount = 0

export type SupportedType =
  | string
  | Date
  | null
  | number
  | Buffer
  | typeFest.JsonArray
  | typeFest.JsonObject

export type QueryMetadata = {
  queryName: string
  queryText: string
}

export function defineMultiTransform<
  SpecificDatabase extends db.Database,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Transform extends unknown
>(
  queryText: string,
  transform: (rows: Row[]) => Transform
): ((
  database: SpecificDatabase,
  ...values: Values
) => Promise<Transform>) &
  QueryMetadata {
  // TODO Switch from function to value
  const queryName = `q${globalQueryNameCount.toString(
    queryNameBase
  )}`
  globalQueryNameCount += 1
  const query = (async (database, ...values) => {
    const result = await database.pool.query<Row, Values>(
      {
        name: queryName,
        text: queryText,
      },
      values
    )
    return transform(result.rows)
  }) as ((
    database: SpecificDatabase,
    ...values: Values
  ) => Promise<Transform>) &
    QueryMetadata
  query.queryName = queryName
  query.queryText = queryText
  return query
}

export function defineMulti<
  SpecificDatabase extends db.Database,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  queryText: string
): ((
  database: SpecificDatabase,
  ...values: Values
) => Promise<Row[]>) &
  QueryMetadata {
  return defineMultiTransform<
    SpecificDatabase,
    Values,
    Row,
    Row[]
  >(queryText, (rows) => rows)
}

export function defineOptional<
  SpecificDatabase extends db.Database,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  queryText: string
): ((
  database: SpecificDatabase,
  ...values: Values
) => Promise<Row | undefined>) &
  QueryMetadata {
  return defineMultiTransform<
    SpecificDatabase,
    Values,
    Row,
    Row | undefined
  >(queryText, (rows) => {
    if (rows.length === 0) {
      return undefined
    } else if (rows.length === 1) {
      return rows[0]
    } else {
      throw new Error(
        `Too many rows returned (${rows.length})`
      )
    }
  })
}

export function defineSingle<
  SpecificDatabase extends db.Database,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  queryText: string
): ((
  database: SpecificDatabase,
  ...values: Values
) => Promise<Row>) &
  QueryMetadata {
  return defineMultiTransform<
    SpecificDatabase,
    Values,
    Row,
    Row
  >(queryText, (rows) => {
    if (rows.length === 0) {
      throw new Error('No rows returned')
    } else if (rows.length === 1) {
      return rows[0]
    } else {
      throw new Error(
        `Too many rows returned (${rows.length})`
      )
    }
  })
}

export function defineVoid<
  SpecificDatabase extends db.Database,
  Values extends SupportedType[]
>(
  queryText: string
): ((
  database: SpecificDatabase,
  ...values: Values
) => Promise<void>) &
  QueryMetadata {
  return defineMultiTransform<
    SpecificDatabase,
    Values,
    Record<string, never>,
    void
  >(queryText, (rows) => {
    if (rows.length === 0) {
      return
    } else {
      throw new Error(
        `Too many rows returned (${rows.length})`
      )
    }
  })
}

export type QueryError = Error & { code: string }

export function isQueryError(
  error: unknown
): error is QueryError {
  return (
    error instanceof Error &&
    typeof (error as QueryError).code === 'string'
  )
}

// https://www.postgresql.org/docs/13/errcodes-appendix.html
export const errorCode = {
  uniqueViolation: '23505',
} as const

const uniqueViolationConstraintNamePattern = / unique constraint "([^"]*)"$/

export function getUniqueViolationConstraintName(
  error: QueryError
): string | undefined {
  return uniqueViolationConstraintNamePattern.exec(
    error.message
  )?.[1]
}

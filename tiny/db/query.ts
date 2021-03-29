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

export function defineMulti<
  SpecificDatabase extends db.Database,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  text: string
): (
  database: SpecificDatabase,
  ...values: Values
) => Promise<Row[]> {
  const name = `q${globalQueryNameCount.toString(
    queryNameBase
  )}`
  globalQueryNameCount += 1
  return async (database, ...values) => {
    const result = await database.pool.query<Row, Values>(
      {
        name,
        text,
      },
      values
    )
    return result.rows
  }
}

export function defineOptional<
  SpecificDatabase extends db.Database,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  text: string
): (
  database: SpecificDatabase,
  ...values: Values
) => Promise<Row | undefined> {
  const base = defineMulti<SpecificDatabase, Values, Row>(
    text
  )
  return async (database, ...values) => {
    const result = await base(database, ...values)
    if (result.length === 0) {
      return undefined
    } else if (result.length === 1) {
      return result[0]
    } else {
      throw new Error(
        `Too many rows returned (${result.length})`
      )
    }
  }
}

export function defineSingle<
  SpecificDatabase extends db.Database,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  text: string
): (
  database: SpecificDatabase,
  ...values: Values
) => Promise<Row> {
  const base = defineOptional<
    SpecificDatabase,
    Values,
    Row
  >(text)
  return async (database, ...values) => {
    const result = await base(database, ...values)
    if (result === undefined) {
      throw new Error('No rows returned')
    } else {
      return result
    }
  }
}

export function defineVoid<
  SpecificDatabase extends db.Database,
  Values extends SupportedType[]
>(
  text: string
): (
  database: SpecificDatabase,
  ...values: Values
) => Promise<void> {
  const base = defineMulti<
    SpecificDatabase,
    Values,
    Record<string, never>
  >(text)
  return async (database, ...values) => {
    const result = await base(database, ...values)
    if (result.length === 0) {
      return
    } else {
      throw new Error(
        `Too many rows returned (${result.length})`
      )
    }
  }
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

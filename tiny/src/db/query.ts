import * as errorModule from '@tiny/core/error.ts'
import * as payload from '@tiny/core/payload.ts'
import * as time from '@tiny/core/time.ts'
import ms from 'ms'
import type * as typeFest from 'type-fest'

const queryNameBase = 36

let globalQueryNameCount = 0

export type Context<DatabaseId extends symbol> = {
  databases: { [K in DatabaseId]: Database<K> }
}

export type SupportedType =
  | string
  | Date
  | null
  | number
  | Buffer
  | typeFest.JsonArray
  | typeFest.JsonObject

export type Database<DatabaseId extends symbol> = {
  query<
    Values extends SupportedType[],
    Row extends Record<string, SupportedType>,
    Transform
  >(
    query: Query<DatabaseId, Values, Row, Transform>,
    ...values: Values
  ): Promise<Transform>
}

export type Query<
  DatabaseId extends symbol,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Transform
> = {
  databaseId: DatabaseId
  name: string
  text: string
  transform: (rows: Row[]) => Transform
  values?: Values
  conflictMap?: Record<string, string>
}

type QueryMeta<
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
> = {
  values?: Values
  row?: Row
  conflictMap?: Record<string, string>
}

export function values<Values extends SupportedType[]>():
  | Values
  | undefined {
  return undefined
}

export function row<
  Row extends Record<string, SupportedType>
>(): Row | undefined {
  return undefined
}

export function defineMultiTransform<
  DatabaseId extends symbol,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Transform
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Values, Row>,
  transform: (rows: Row[]) => Transform
): Query<DatabaseId, Values, Row, Transform> {
  const name = `q${globalQueryNameCount.toString(
    queryNameBase
  )}`
  globalQueryNameCount += 1
  return {
    databaseId,
    name,
    text,
    transform,
    ...meta,
  }
}

export function defineMulti<
  DatabaseId extends symbol,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Values, Row>
): Query<DatabaseId, Values, Row, Row[]> {
  return defineMultiTransform(
    databaseId,
    text,
    meta,
    (rows) => rows
  )
}

export function defineOptional<
  DatabaseId extends symbol,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Values, Row>
): Query<DatabaseId, Values, Row, Row | undefined> {
  return defineMultiTransform(
    databaseId,
    text,
    meta,
    (rows) => {
      if (rows.length === 0) {
        return undefined
      } else if (rows.length === 1) {
        return rows[0]
      } else {
        throw new Error(
          `Too many rows returned (${rows.length})`
        )
      }
    }
  )
}

export function defineSingle<
  DatabaseId extends symbol,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Values, Row>
): Query<DatabaseId, Values, Row, Row> {
  return defineMultiTransform(
    databaseId,
    text,
    meta,
    (rows) => {
      if (rows.length === 0) {
        throw new Error('No rows returned')
      } else if (rows.length === 1) {
        return rows[0]
      } else {
        throw new Error(
          `Too many rows returned (${rows.length})`
        )
      }
    }
  )
}

export function defineVoid<
  DatabaseId extends symbol,
  Values extends SupportedType[]
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Values, Record<string, SupportedType>>
): Query<
  DatabaseId,
  Values,
  Record<string, SupportedType>,
  void
> {
  return defineMultiTransform(
    databaseId,
    text,
    meta,
    (rows) => {
      if (rows.length === 0) {
        return
      } else {
        throw new Error(
          `Too many rows returned (${rows.length})`
        )
      }
    }
  )
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

const uniqueViolationConstraintNamePattern =
  / unique constraint "([^"]*)"$/

export function getUniqueViolationConstraintName(
  error: QueryError
): string | undefined {
  return uniqueViolationConstraintNamePattern.exec(
    error.message
  )?.[1]
}

export async function retryDbQuery<
  DatabaseId extends symbol,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Transform
>(
  ctx: errorModule.Context & Context<DatabaseId>,
  description: string,
  query: Query<DatabaseId, Values, Row, Transform>,
  ...values: Values
): Promise<Transform> {
  return await errorModule.retry(
    ctx,
    () =>
      ctx.databases[query.databaseId].query(
        query,
        ...values
      ),
    {
      maxAttempts: 15,
      minDelayMs: time.interval({ milliseconds: 10 }),
      windowMs: time.interval({ seconds: 30 }),
      onError(error, attempt, nextDelayMs) {
        if (
          query.conflictMap &&
          isQueryError(error) &&
          error.code === errorCode.uniqueViolation
        ) {
          const constraintName =
            getUniqueViolationConstraintName(error)
          const conflictField =
            constraintName &&
            query.conflictMap[constraintName]
          if (conflictField) {
            throw new payload.ConflictError(
              conflictField as never
            )
          }
        }
        console.error(
          `Retrying ${description} query (attempt ${attempt}, delay ${ms(
            nextDelayMs
          )})`,
          error
        )
      },
    }
  )
}

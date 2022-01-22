import * as tinyError from '@tiny/core/error.ts'
import * as tinyPayload from '@tiny/core/payload.ts'
import ms from 'ms'
import type * as typeFest from 'type-fest'

const queryNameBase = 36

let globalQueryNameCount = 0

export type Context<DatabaseId extends symbol> = {
  databases: { [K in DatabaseId]: Database<K> }
  databaseRetryConfig: Omit<
    tinyError.RetryConfig,
    'onError'
  >
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
    Params extends SupportedType[],
    Row extends Record<string, SupportedType>,
    Result
  >(
    query: Query<DatabaseId, Params, Row, Result>,
    ...params: Params
  ): Promise<Result>
}

export type Query<
  DatabaseId extends symbol,
  Params extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Result
> = {
  databaseId: DatabaseId
  name: string
  text: string
  transform: (rows: Row[]) => Result
  params?: Params
  conflictMap?: Record<string, string>
}

type QueryMeta<
  Params extends SupportedType[],
  Row extends Record<string, SupportedType>
> = {
  params?: Params
  row?: Row
  conflictMap?: Record<string, string>
}

export function params<Params extends SupportedType[]>():
  | Params
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
  Params extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Result
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Params, Row>,
  transform: (rows: Row[]) => Result
): Query<DatabaseId, Params, Row, Result> {
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
  Params extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Params, Row>
): Query<DatabaseId, Params, Row, Row[]> {
  return defineMultiTransform(
    databaseId,
    text,
    meta,
    (rows) => rows
  )
}

export function defineOptional<
  DatabaseId extends symbol,
  Params extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Params, Row>
): Query<DatabaseId, Params, Row, Row | undefined> {
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
  Params extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Params, Row>
): Query<DatabaseId, Params, Row, Row> {
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
  Params extends SupportedType[]
>(
  databaseId: DatabaseId,
  text: string,
  meta: QueryMeta<Params, Record<string, SupportedType>>
): Query<
  DatabaseId,
  Params,
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

export async function retryQuery<
  DatabaseId extends symbol,
  Params extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Result
>(
  ctx: tinyError.Context & Context<DatabaseId>,
  description: string,
  query: Query<DatabaseId, Params, Row, Result>,
  ...params: Params
): Promise<Result> {
  try {
    return await tinyError.retry(
      ctx,
      () =>
        ctx.databases[query.databaseId].query(
          query,
          ...params
        ),
      {
        ...ctx.databaseRetryConfig,
        onError(error, attempt, nextDelayMs) {
          if (
            isQueryError(error) &&
            error.code === errorCode.uniqueViolation
          ) {
            throw error
          }
          console.error(
            `Retrying ${description} DB query (attempt ${attempt}, delay ${ms(
              nextDelayMs
            )})`,
            error
          )
        },
      }
    )
  } catch (error) {
    if (
      query.conflictMap &&
      isQueryError(error) &&
      error.code === errorCode.uniqueViolation
    ) {
      const constraintName =
        getUniqueViolationConstraintName(error)
      const conflictField =
        constraintName && query.conflictMap[constraintName]
      if (conflictField) {
        throw new tinyPayload.ConflictError(
          conflictField as never
        )
      }
    }
    throw error
  }
}

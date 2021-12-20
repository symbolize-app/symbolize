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

export type BaseDatabase<Id> =
  Database<Id> extends typeFest.Opaque<infer Type, Id>
    ? Type
    : never

export type Database<Id> = typeFest.Opaque<
  {
    query<
      Values extends SupportedType[],
      Row extends Record<string, SupportedType>,
      Transform
    >(
      query: Query<Id, Values, Row, Transform>,
      ...values: Values
    ): Promise<Transform>
  },
  Id
>

export type BaseQuery<
  DatabaseId,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Transform
> = Query<
  DatabaseId,
  Values,
  Row,
  Transform
> extends typeFest.Opaque<infer Type, [DatabaseId, Values]>
  ? Type
  : never

export type Query<
  DatabaseId,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Transform
> = typeFest.Opaque<
  {
    name: string
    text: string
    transform: (rows: Row[]) => Transform
  },
  [DatabaseId, Values]
>

export function createDatabase<Id>(
  database: BaseDatabase<Id>
): Database<Id> {
  return database as Database<Id>
}

export function createQuery<
  DatabaseId,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Transform
>(
  query: BaseQuery<DatabaseId, Values, Row, Transform>
): Query<DatabaseId, Values, Row, Transform> {
  return query as Query<DatabaseId, Values, Row, Transform>
}

export function defineMultiTransform<
  DatabaseId,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>,
  Transform
>(
  text: string,
  transform: (rows: Row[]) => Transform
): Query<DatabaseId, Values, Row, Transform> {
  const name = `q${globalQueryNameCount.toString(
    queryNameBase
  )}`
  globalQueryNameCount += 1
  return createQuery({
    name,
    text,
    transform,
  })
}

export function defineMulti<
  DatabaseId,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  queryText: string
): Query<DatabaseId, Values, Row, Row[]> {
  return defineMultiTransform<
    DatabaseId,
    Values,
    Row,
    Row[]
  >(queryText, (rows) => rows)
}

export function defineOptional<
  DatabaseId,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(
  queryText: string
): Query<DatabaseId, Values, Row, Row | undefined> {
  return defineMultiTransform<
    DatabaseId,
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
  DatabaseId,
  Values extends SupportedType[],
  Row extends Record<string, SupportedType>
>(queryText: string): Query<DatabaseId, Values, Row, Row> {
  return defineMultiTransform<DatabaseId, Values, Row, Row>(
    queryText,
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
  DatabaseId,
  Values extends SupportedType[]
>(
  queryText: string
): Query<
  DatabaseId,
  Values,
  Record<string, SupportedType>,
  void
> {
  return defineMultiTransform<
    DatabaseId,
    Values,
    Record<string, SupportedType>,
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

const uniqueViolationConstraintNamePattern =
  / unique constraint "([^"]*)"$/

export function getUniqueViolationConstraintName(
  error: QueryError
): string | undefined {
  return uniqueViolationConstraintNamePattern.exec(
    error.message
  )?.[1]
}

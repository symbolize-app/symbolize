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
  return async (
    database: SpecificDatabase,
    ...values: Values
  ) => {
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
  return async (
    database: SpecificDatabase,
    ...values: Values
  ) => {
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
  return async (
    database: SpecificDatabase,
    ...values: Values
  ) => {
    const result = await base(database, ...values)
    if (result === undefined) {
      throw new Error('No rows returned')
    } else {
      return result
    }
  }
}

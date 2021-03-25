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

export function define<
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

import type * as db from '@fe/db/index.ts'
import memberFind from '@fe/db/query/memberFind.sql'
import * as query from '@tiny/db/query.ts'

export const find = query.defineOptional<
  db.DatabaseApiRead,
  [id: Buffer],
  { email: string }
>(memberFind)

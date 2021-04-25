import type * as db from '@fe/db/index.ts'
import memberCreate from '@fe/db/query/memberCreate.sql'
import memberFind from '@fe/db/query/memberFind.sql'
import * as query from '@tiny/db/query.ts'

export const find = query.defineOptional<
  db.Read,
  [id: Buffer],
  { email: string }
>(memberFind)

export const create = query.defineVoid<
  db.Write,
  [id: Buffer, email: string, handle: string]
>(memberCreate)

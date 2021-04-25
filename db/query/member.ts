import type * as appQuery from '@fe/db/query/index.ts'
import appQueryMemberCreate from '@fe/db/query/memberCreate.sql'
import appQueryMemberFind from '@fe/db/query/memberFind.sql'
import * as query from '@tiny/db/query.ts'

export const find = query.defineOptional<
  appQuery.Read,
  [id: Buffer],
  { email: string }
>(appQueryMemberFind)

export const create = query.defineVoid<
  appQuery.Write,
  [id: Buffer, email: string, handle: string]
>(appQueryMemberCreate)

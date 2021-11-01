import appQueryMemberCreate from '@db/query/memberCreate.sql'
import appQueryMemberFind from '@db/query/memberFind.sql'
import type * as appQuery from '@fe/db/query/index.ts'
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

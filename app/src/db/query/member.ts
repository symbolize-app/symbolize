import dbQueryMemberCreate from '@db/query/member_create.sql'
import dbQueryMemberFind from '@db/query/member_find.sql'
import type * as appQuery from '@fe/db/query/index.ts'
import * as query from '@tiny/db/query.ts'

export const find = query.defineOptional<
  appQuery.Read,
  [id: Buffer],
  { email: string }
>(dbQueryMemberFind)

export const create = query.defineVoid<
  appQuery.Write,
  [id: Buffer, email: string, handle: string]
>(dbQueryMemberCreate)

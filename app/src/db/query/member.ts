import dbQueryMemberCreate from '@db/query/member_create.sql'
import dbQueryMemberFind from '@db/query/member_find.sql'
import type * as appDbQuery from '@fe/db/query/index.ts'
import * as dbQuery from '@tiny/db/query.ts'

export const find = dbQuery.defineOptional<
  appDbQuery.Read,
  [id: Buffer],
  { email: string }
>(dbQueryMemberFind, {})

export const create = dbQuery.defineVoid<
  appDbQuery.Write,
  [id: Buffer, email: string, handle: string]
>(dbQueryMemberCreate, {
  conflictMap: {
    ['member_email_key']: 'email',
    ['member_handle_key']: 'handle',
  },
})

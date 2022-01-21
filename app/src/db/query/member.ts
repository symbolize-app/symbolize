import dbQueryMemberCreate from '@db/query/member_create.sql'
import dbQueryMemberFind from '@db/query/member_find.sql'
import * as appDbQuery from '@fe/db/query/index.ts'
import * as dbQuery from '@tiny/db/query.ts'

export const find = dbQuery.defineOptional(
  appDbQuery.read,
  dbQueryMemberFind,
  {
    params: dbQuery.params<[id: Buffer]>(),
    row: dbQuery.row<{ email: string }>(),
  }
)

export const create = dbQuery.defineVoid(
  appDbQuery.write,
  dbQueryMemberCreate,
  {
    params:
      dbQuery.params<
        [id: Buffer, email: string, handle: string]
      >(),
    conflictMap: {
      ['member_email_key']: 'email',
      ['member_handle_key']: 'handle',
    },
  }
)

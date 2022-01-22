import * as appDbQuery from '@app/db/query/index.ts'
import dbQueryMemberCreate from '@db/query/member_create.sql'
import dbQueryMemberFind from '@db/query/member_find.sql'
import * as tinyDbQuery from '@tiny/db/query.ts'

export const find = tinyDbQuery.defineOptional(
  appDbQuery.read,
  dbQueryMemberFind,
  {
    params: tinyDbQuery.params<[id: Buffer]>(),
    row: tinyDbQuery.row<{ email: string }>(),
  }
)

export const create = tinyDbQuery.defineVoid(
  appDbQuery.write,
  dbQueryMemberCreate,
  {
    params:
      tinyDbQuery.params<
        [id: Buffer, email: string, handle: string]
      >(),
    conflictMap: {
      ['member_email_key']: 'email',
      ['member_handle_key']: 'handle',
    },
  }
)

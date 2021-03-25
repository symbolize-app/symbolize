import type * as db from '@fe/db/index.ts'
import * as query from '@tiny/db/query.ts'

export const find = query.define<
  db.DatabaseApiRead,
  [id: Buffer],
  { email: string }
>(
  'SELECT email as "email" FROM member WHERE member.id = $1'
)

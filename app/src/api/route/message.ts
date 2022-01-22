import * as appMessage from '@fe/core/message.ts'
import type * as appDbQuery from '@fe/db/query/index.ts'
import * as appDbQueryMember from '@fe/db/query/member.ts'
import * as tinyRoute from '@tiny/api/route.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinyDbQuery from '@tiny/db/query.ts'

export const show = tinyRoute.defineBase<
  tinyError.Context & appDbQuery.ReadContext
>(['GET'], /^\/api\/message$/, async (ctx) => {
  const row = await tinyDbQuery.retryQuery(
    ctx,
    'member find',
    appDbQueryMember.find,
    Buffer.from('ABCD', 'hex')
  )
  return {
    status: 200,
    headers: {
      'content-type': 'text/plain',
    },
    body: `${appMessage.hi} ${JSON.stringify(row)}`,
  }
})

export const routes = [show]

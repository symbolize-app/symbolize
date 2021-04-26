import * as appRoute from '@fe/api/route/index.ts'
import * as appMessage from '@fe/core/message.ts'
import type * as appQuery from '@fe/db/query/index.ts'
import * as appQueryMember from '@fe/db/query/member.ts'
import * as route from '@tiny/api/route.ts'
import type * as errorModule from '@tiny/core/error.ts'

export const show = route.define<
  errorModule.Context & appQuery.ReadContext
>(['GET'], /^\/api\/message$/, async (ctx) => {
  const row = await appRoute.retryQuery(
    ctx,
    'member find',
    () =>
      ctx.databaseApiRead.query(
        appQueryMember.find,
        Buffer.from('ABCD', 'hex')
      )
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

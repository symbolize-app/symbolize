import * as appRoute from '@fe/api/route/index.ts'
import * as appEndpoint from '@fe/core/endpoint.ts'
import type * as appQuery from '@fe/db/query/index.ts'
import * as appQueryMember from '@fe/db/query/member.ts'
import * as route from '@tiny/api/route.ts'
import * as crypto from '@tiny/core/crypto.node.ts'
import type * as errorModule from '@tiny/core/error.ts'

export const create = route.defineEndpoint<
  errorModule.Context & appQuery.WriteContext
>(appEndpoint.memberCreate, async (ctx, request) => {
  const requestObject = await appRoute.checkRequestJson(
    appEndpoint.memberCreate,
    request
  )
  const id = crypto.hash(
    Buffer.from(requestObject.requestId, 'hex')
  )
  await appRoute.retryConflictQuery(
    ctx,
    'member create',
    appEndpoint.memberCreate,
    {
      ['member_email_key']: 'email',
      ['member_handle_key']: 'handle',
    },
    () =>
      ctx.databaseApiWrite.query(
        appQueryMember.create,
        id,
        requestObject.email,
        requestObject.handle
      )
  )
  return appRoute.checkOkResponse(
    appEndpoint.memberCreate,
    {
      id: id.toString('hex'),
    }
  )
})

export const routes = [create]

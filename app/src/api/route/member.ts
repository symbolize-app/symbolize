import * as appRoute from '@fe/api/route/index.ts'
import * as appEndpointMember from '@fe/core/endpoint/member.ts'
import type * as appQuery from '@fe/db/query/index.ts'
import * as appQueryMember from '@fe/db/query/member.ts'
import * as route from '@tiny/api/route.ts'
import * as crypto from '@tiny/core/crypto.node.ts'
import type * as errorModule from '@tiny/core/error.ts'

export const create = route.defineEndpoint<
  errorModule.Context & appQuery.WriteContext
>(appEndpointMember.create, async (ctx, request) => {
  const requestData = await appRoute.checkRequestJson(
    appEndpointMember.create,
    request
  )
  const id = crypto.hash(
    Buffer.from(requestData.requestId, 'hex')
  )
  const { email, handle } = requestData
  await appRoute.retryConflictQuery(
    ctx,
    ctx.databaseApiWrite,
    'member create',
    appEndpointMember.create,
    {
      ['member_email_key']: 'email',
      ['member_handle_key']: 'handle',
    },
    appQueryMember.create,
    id,
    email,
    handle
  )
  return appRoute.checkOkResponse(
    appEndpointMember.create,
    {
      id: id.toString('hex'),
    }
  )
})

export const routes = [create]

import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'

export type Create = typeof create
export const create = endpoint.defineConflictPostEndpoint(
  '/api/member/create',
  payload.checkObject({
    requestId: appPayload.checkId,
    email: appPayload.checkEmail,
    handle: appPayload.checkHandle,
  }),
  payload.checkObject({
    id: appPayload.checkId,
  }),
  payload.checkConflictResponse('email', 'handle')
)

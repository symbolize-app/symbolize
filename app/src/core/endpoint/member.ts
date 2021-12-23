import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'

export type Create = typeof create
export const create = endpoint.definePostEndpoint(
  '/api/member/create',
  {
    requestJson: payload.object({
      requestId: appPayload.id,
      email: appPayload.email,
      handle: appPayload.handle,
    }),
    okResponseJson: payload.object({
      id: appPayload.id,
    }),
    conflictResponseJson: payload.conflict(
      'email',
      'handle'
    ),
  }
)

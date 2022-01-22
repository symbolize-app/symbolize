import * as appPayload from '@fe/core/payload.ts'
import * as tinyEndpoint from '@tiny/core/endpoint.ts'
import * as tinyPayload from '@tiny/core/payload.ts'

export type Create = typeof create
export const create = tinyEndpoint.definePostEndpoint(
  '/api/member/create',
  {
    requestJson: tinyPayload.object({
      requestId: appPayload.id,
      email: appPayload.email,
      handle: appPayload.handle,
    }),
    okResponseJson: tinyPayload.object({
      id: appPayload.id,
    }),
    conflictResponseJson: tinyPayload.conflict(
      'email',
      'handle'
    ),
  }
)

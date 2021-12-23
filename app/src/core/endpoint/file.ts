import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'

export type Create = typeof create
export const create = endpoint.definePostEndpoint(
  '/api/file/create',
  {
    checkRequestParams: payload.object({
      requestId: appPayload.id,
    }),
  }
)

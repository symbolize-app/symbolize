import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'

export type Write = typeof write
export const write = endpoint.definePostEndpoint(
  '/api/file/write',
  {
    requestParams: payload.object({
      requestId: appPayload.id,
    }),
    requestBytes: true,
    okResponseJson: payload.object({
      id: appPayload.id,
    }),
  }
)

export type Read = typeof read
export const read = endpoint.defineGetEndpoint(
  '/api/file/read',
  {
    requestParams: payload.object({
      id: appPayload.id,
    }),
    okResponseStream: true,
  }
)

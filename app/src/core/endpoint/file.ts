import * as appPayload from '@fe/core/payload.ts'
import * as tinyEndpoint from '@tiny/core/endpoint.ts'
import * as tinyPayload from '@tiny/core/payload.ts'

export type Write = typeof write
export const write = tinyEndpoint.definePostEndpoint(
  '/api/file/write',
  {
    requestParams: tinyPayload.object({
      requestId: appPayload.id,
    }),
    requestBytes: true,
    okResponseJson: tinyPayload.object({
      id: appPayload.id,
    }),
  }
)

export type Read = typeof read
export const read = tinyEndpoint.defineGetEndpoint(
  '/api/file/read',
  {
    requestParams: tinyPayload.object({
      id: appPayload.id,
    }),
    okResponseStream: true,
  }
)

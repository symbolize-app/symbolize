import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'
import type * as typeFest from 'type-fest'

export type Query = typeof query
export type QueryResult = typeFest.IterableElement<
  endpoint.OkResponseData<Query>['results']
>
export const query = endpoint.defineGetEndpoint(
  '/api/search/query',
  payload.checkObject({
    language: payload.checkString({
      min: 2,
      max: 16,
    }) /* TODO enum */,
    query: payload.checkString({
      min: 0,
      max: 256,
    }),
  }),
  payload.checkObject({
    results: payload.checkArray(
      payload.checkObject({
        id: appPayload.checkId,
        updatedAt: payload.checkTimestamp,
        content: appPayload.checkContent,
        /* TODO more */
      })
    ),
  })
)

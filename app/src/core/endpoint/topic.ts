import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'
import type * as typeFest from 'type-fest'

export type Create = typeof create
export const create = endpoint.defineConflictPostEndpoint(
  '/api/topic/create',
  payload.checkObject({
    requestId: appPayload.checkId,
    memberId: appPayload.checkId,
    title: appPayload.checkTitle,
    slug: appPayload.checkSlug,
    content: appPayload.checkContent,
  }),
  payload.checkObject({
    id: appPayload.checkId,
  }),
  payload.checkConflictResponse('slug')
)

export type List = typeof list
export type ListResult = typeFest.IterableElement<
  endpoint.OkResponse<List>['results']
>
export const list = endpoint.defineGetEndpoint(
  '/api/topic/list',
  payload.checkObject({}),
  payload.checkObject({
    results: payload.checkArray(
      payload.checkObject({
        id: appPayload.checkId,
        updated: payload.checkTimestamp,
        title: appPayload.checkTitle,
        slug: appPayload.checkSlug,
        content: appPayload.checkContent,
      })
    ),
  })
)

export type Update = typeof update
export const update = endpoint.defineConflictPostEndpoint(
  '/api/topic/update',
  payload.checkObject({
    id: appPayload.checkId,
    updatedOld: payload.checkTimestamp,
    title: appPayload.checkTitle,
    slug: appPayload.checkSlug,
    content: appPayload.checkContent,
  }),
  payload.checkObject({
    updated: payload.checkTimestamp,
  }),
  payload.checkConflictResponse('slug')
)

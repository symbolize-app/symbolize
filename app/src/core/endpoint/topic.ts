import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'
import type * as typeFest from 'type-fest'

export type Create = typeof create
export const create = endpoint.definePostEndpoint(
  '/api/topic/create',
  {
  requestJson: payload.object({
    requestId: appPayload.id,
    memberId: appPayload.id,
    title: appPayload.title,
    slug: appPayload.slug,
    content: appPayload.content,
  }),
  okResponseJson: payload.object({
    id: appPayload.id,
  }),
  conflictResponseJson: payload.conflict('slug')
  },
)

export type List = typeof list
export type ListResult = typeFest.IterableElement<
  payload.Payload<List['okResponseJson']>['results']
>
export const list = endpoint.defineGetEndpoint(
  '/api/topic/list',
  {
  requestParams: payload.object({}),
  okResponseJson: payload.object({
    results: payload.array(
      payload.object({
        id: appPayload.id,
        updatedAt: payload.timestamp,
        title: appPayload.title,
        slug: appPayload.slug,
        content: appPayload.content,
      })
    ),
  })}
)

export type Update = typeof update
export const update = endpoint.definePostEndpoint(
  '/api/topic/update',
  {
  requestJson: payload.object({
    id: appPayload.id,
    updatedOld: payload.timestamp,
    title: appPayload.title,
    slug: appPayload.slug,
    content: appPayload.content,
  }),
  okResponseJson: payload.object({
    updated: payload.timestamp,
  }),
  conflictResponseJson: payload.conflict('slug')}
)

import * as appPayload from '@fe/core/payload.ts'
import * as tinyEndpoint from '@tiny/core/endpoint.ts'
import * as tinyPayload from '@tiny/core/payload.ts'
import type * as typeFest from 'type-fest'

export type Create = typeof create
export const create = tinyEndpoint.definePostEndpoint(
  '/api/topic/create',
  {
    requestJson: tinyPayload.object({
      requestId: appPayload.id,
      memberId: appPayload.id,
      title: appPayload.title,
      slug: appPayload.slug,
      content: appPayload.content,
    }),
    okResponseJson: tinyPayload.object({
      id: appPayload.id,
    }),
    conflictResponseJson: tinyPayload.conflict('slug'),
  }
)

export type List = typeof list
export type ListResult = typeFest.IterableElement<
  tinyPayload.Payload<List['okResponseJson']>['results']
>
export const list = tinyEndpoint.defineGetEndpoint(
  '/api/topic/list',
  {
    requestParams: tinyPayload.object({}),
    okResponseJson: tinyPayload.object({
      results: tinyPayload.array(
        tinyPayload.object({
          id: appPayload.id,
          updatedAt: tinyPayload.timestamp,
          title: appPayload.title,
          slug: appPayload.slug,
          content: appPayload.content,
        })
      ),
    }),
  }
)

export type Update = typeof update
export const update = tinyEndpoint.definePostEndpoint(
  '/api/topic/update',
  {
    requestJson: tinyPayload.object({
      id: appPayload.id,
      updatedOld: tinyPayload.timestamp,
      title: appPayload.title,
      slug: appPayload.slug,
      content: appPayload.content,
    }),
    okResponseJson: tinyPayload.object({
      updated: tinyPayload.timestamp,
    }),
    conflictResponseJson: tinyPayload.conflict('slug'),
  }
)

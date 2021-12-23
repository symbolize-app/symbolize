import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'
import type * as typeFest from 'type-fest'

export type Query = typeof query
export type QueryResult = typeFest.IterableElement<
  payload.Payload<Query['okResponseJson']>['results']
>
export const query = endpoint.defineGetEndpoint(
  '/api/search/query',
  {
    requestParams: payload.object({
      language: appPayload.language,
      query: payload.string({
        min: 0,
        max: 256,
      }),
    }),
    okResponseJson: payload.object({
      results: payload.array(
        payload.object({
          type: appPayload.documentType,
          id: appPayload.id,
          createdAt: payload.timestamp,
          createdBy: appPayload.id,
          updatedAt: payload.timestamp,
          subforumId: payload.nullOr(appPayload.id),
          topicId: payload.nullOr(appPayload.id),
          taxonRank: payload.nullOr(appPayload.taxonRank),
          parents: payload.array(appPayload.id),
          title: payload.nullOr(appPayload.title),
          names: payload.array(appPayload.name),
          slug: appPayload.slug,
          tags: payload.array(appPayload.id),
          content: appPayload.content,
        })
      ),
    }),
  }
)

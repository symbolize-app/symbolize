import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as tinyPayload from '@tiny/core/payload.ts'
import type * as typeFest from 'type-fest'

export type Query = typeof query
export type QueryResult = typeFest.IterableElement<
  tinyPayload.Payload<Query['okResponseJson']>['results']
>
export const query = endpoint.defineGetEndpoint(
  '/api/search/query',
  {
    requestParams: tinyPayload.object({
      language: appPayload.language,
      query: tinyPayload.string({
        min: 0,
        max: 256,
      }),
    }),
    okResponseJson: tinyPayload.object({
      results: tinyPayload.array(
        tinyPayload.object({
          type: appPayload.documentType,
          id: appPayload.id,
          createdAt: tinyPayload.timestamp,
          createdBy: appPayload.id,
          updatedAt: tinyPayload.timestamp,
          subforumId: tinyPayload.nullOr(appPayload.id),
          topicId: tinyPayload.nullOr(appPayload.id),
          taxonRank: tinyPayload.nullOr(
            appPayload.taxonRank
          ),
          parents: tinyPayload.array(appPayload.id),
          title: tinyPayload.nullOr(appPayload.title),
          names: tinyPayload.array(appPayload.name),
          slug: appPayload.slug,
          tags: tinyPayload.array(appPayload.id),
          content: appPayload.content,
        })
      ),
    }),
  }
)

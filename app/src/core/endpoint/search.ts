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
    language: appPayload.checkLanguage,
    query: payload.checkString({
      min: 0,
      max: 256,
    }),
  }),
  payload.checkObject({
    results: payload.checkArray(
      payload.checkObject({
        type: appPayload.checkDocumentType,
        id: appPayload.checkId,
        createdAt: payload.checkTimestamp,
        createdBy: appPayload.checkId,
        updatedAt: payload.checkTimestamp,
        subforumId: payload.checkNull(appPayload.checkId),
        topicId: payload.checkNull(appPayload.checkId),
        taxonRank: payload.checkNull(
          appPayload.checkTaxonRank
        ),
        parents: payload.checkArray(appPayload.checkId),
        title: payload.checkNull(appPayload.checkTitle),
        names: payload.checkArray(appPayload.checkName),
        slug: appPayload.checkSlug,
        tags: payload.checkArray(appPayload.checkId),
        content: appPayload.checkContent,
      })
    ),
  })
)

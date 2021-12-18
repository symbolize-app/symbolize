import * as appRoute from '@fe/api/route/index.ts'
import * as appEndpointSearch from '@fe/core/endpoint/search.ts'
import * as appPayload from '@fe/core/payload.ts'
import * as appSubmit from '@fe/core/submit.ts'
import * as route from '@tiny/api/route.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as payload from '@tiny/core/payload.ts'
import type * as submit from '@tiny/core/submit.ts'

export const query = route.defineEndpoint<
  errorModule.Context & submit.Context
>(appEndpointSearch.query, async (ctx, request) => {
  const requestData = appRoute.checkRequestParams(
    appEndpointSearch.query,
    request
  )
  const { results } = await appSubmit.retryGetSubmit(
    ctx,
    'search query',
    ftsQueryEndpoint,
    {
      origin: `http://${process.env.FTS_HOST as string}:${
        process.env.FTS_PORT as string
      }/` /* TODO scheme */,
      headers: {
        ['Authorization']: `Basic ${Buffer.from(
          `:${process.env.FTS_PASSWORD as string}`
        ).toString('base64')}`,
      },
      params: {
        language: requestData.language,
        query: requestData.query,
      },
    }
  )
  return appRoute.checkOkResponse(appEndpointSearch.query, {
    results: results.map((result) => ({
      type: result.type,
      id: result.id,
      createdAt: result.created_at,
      createdBy: result.created_by,
      updatedAt: result.updated_at,
      subforumId: result.subforum_id,
      topicId: result.topic_id,
      taxonRank: result.taxon_rank,
      parents: result.parents,
      title: result.title,
      names: result.names,
      tags: result.tags,
      content: result.content,
    })),
  })
})

export const ftsQueryEndpoint = endpoint.defineGetEndpoint(
  '/query',
  payload.checkObject({
    language: appPayload.checkLanguage,
    query: payload.checkString({
      min: 0,
      max: 256,
    }),
  }),
  payload.checkObject({
    results: payload.checkArray(
      /* eslint-disable @typescript-eslint/naming-convention */
      payload.checkObject({
        type: appPayload.checkDocumentType,
        id: appPayload.checkId,
        created_at: payload.checkTimestamp,
        created_by: appPayload.checkId,
        updated_at: payload.checkTimestamp,
        subforum_id: payload.checkNull(appPayload.checkId),
        topic_id: payload.checkNull(appPayload.checkId),
        taxon_rank: payload.checkNull(
          appPayload.checkTaxonRank
        ),
        parents: payload.checkArray(appPayload.checkId),
        title: payload.checkNull(appPayload.checkTitle),
        names: payload.checkArray(appPayload.checkName),
        tags: payload.checkArray(appPayload.checkId),
        content: appPayload.checkContent,
      })
      /* eslint-enable @typescript-eslint/naming-convention */
    ),
  })
)

export const routes = [query]

import * as appFts from '@fe/api/fts.ts'
import * as appRoute from '@fe/api/route/index.ts'
import * as appEndpointSearch from '@fe/core/endpoint/search.ts'
import * as appSubmit from '@fe/core/submit.ts'
import * as route from '@tiny/api/route.ts'
import type * as errorModule from '@tiny/core/error.ts'
import type * as submit from '@tiny/core/submit.ts'

export const query = route.defineEndpoint<
  errorModule.Context & submit.Context & appFts.Context
>(appEndpointSearch.query, async (ctx, request) => {
  const requestData = appRoute.checkRequestParams(
    appEndpointSearch.query,
    request
  )
  const { results } = await appSubmit.retryGetSubmit(
    ctx,
    'search query',
    appFts.queryEndpoint,
    {
      origin: ctx.fts.origin,
      headers: {
        ['Authorization']: `Basic ${Buffer.from(
          `:${ctx.fts.password}`
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
      slug: result.slug,
      tags: result.tags,
      content: result.content,
    })),
  })
})

export const routes = [query]

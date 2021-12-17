import * as appRoute from '@fe/api/route/index.ts'
import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'
import * as appSubmit from '@fe/core/submit.ts'
import * as appEndpointSearch from '@fe/core/endpoint/search.ts'
import * as route from '@tiny/api/route.ts'
import type * as errorModule from '@tiny/core/error.ts'
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
      origin: `http://${process.env.FTS_HOST}:${process.env.FTS_PORT}/` /* TODO scheme */,
      headers: {
        Authorization: `Basic ${Buffer.from(
          `:${process.env.FTS_PASSWORD}`
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
      id: result.id,
      updatedAt: result.updated_at,
      content: result.content,
    })),
  })
})

export const ftsQueryEndpoint = endpoint.defineGetEndpoint(
  '/query',
  payload.checkObject({
    language: payload.checkString({
      min: 2,
      max: 16,
    }),
    query: payload.checkString({
      min: 0,
      max: 256,
    }),
  }),
  payload.checkObject({
    results: payload.checkArray(
      payload.checkObject({
        id: appPayload.checkId,
        updated_at: payload.checkTimestamp,
        content: appPayload.checkContent,
      })
    ),
  })
)

export const routes = [query]

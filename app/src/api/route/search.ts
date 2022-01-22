import * as appFts from '@fe/api/fts.ts'
import * as appEndpointSearch from '@fe/core/endpoint/search.ts'
import * as tinyRoute from '@tiny/api/route.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinySubmit from '@tiny/core/submit.ts'

export const query = tinyRoute.define(
  appEndpointSearch.query,
  async (
    ctx: tinyError.Context &
      tinySubmit.Context &
      appFts.Context,
    request
  ) => {
    const { results } = (
      await tinySubmit.retrySubmit(
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
            language: request.params.language,
            query: request.params.query,
          },
        }
      )
    ).json
    return {
      status: 200,
      json: {
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
      },
    }
  }
)

export const routes = [query]

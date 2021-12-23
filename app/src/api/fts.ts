import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'

export type Context = {
  fts: {
    origin: string
    password: string
  }
}

export function initContext(): Context {
  return {
    fts: {
      origin: `${process.env.FTS_SCHEME as string}://${
        process.env.FTS_HOST as string
      }:${process.env.FTS_PORT as string}/`,
      password: process.env.FTS_PASSWORD as string,
    },
  }
}

export const queryEndpoint = endpoint.defineGetEndpoint(
  '/query',
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
      /* eslint-disable @typescript-eslint/naming-convention */
      payload.object({
        type: appPayload.documentType,
        id: appPayload.id,
        created_at: payload.timestamp,
        created_by: appPayload.id,
        updated_at: payload.timestamp,
        subforum_id: payload.nullOr(appPayload.id),
        topic_id: payload.nullOr(appPayload.id),
        taxon_rank: payload.nullOr(
          appPayload.taxonRank
        ),
        parents: payload.array(appPayload.id),
        title: payload.nullOr(appPayload.title),
        names: payload.array(appPayload.name),
        slug: appPayload.slug,
        tags: payload.array(appPayload.id),
        content: appPayload.content,
      })
      /* eslint-enable @typescript-eslint/naming-convention */
    ),
  })
}
)

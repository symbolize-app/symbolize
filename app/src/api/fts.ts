import * as appPayload from '@app/core/payload.ts'
import * as tinyEndpoint from '@tiny/core/endpoint.ts'
import * as tinyPayload from '@tiny/core/payload.ts'

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

export const queryEndpoint = tinyEndpoint.defineGetEndpoint(
  '/query',
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
        /* eslint-disable @typescript-eslint/naming-convention */
        tinyPayload.object({
          type: appPayload.documentType,
          id: appPayload.id,
          created_at: tinyPayload.timestamp,
          created_by: appPayload.id,
          updated_at: tinyPayload.timestamp,
          subforum_id: tinyPayload.nullOr(appPayload.id),
          topic_id: tinyPayload.nullOr(appPayload.id),
          taxon_rank: tinyPayload.nullOr(
            appPayload.taxonRank
          ),
          parents: tinyPayload.array(appPayload.id),
          title: tinyPayload.nullOr(appPayload.title),
          names: tinyPayload.array(appPayload.name),
          slug: appPayload.slug,
          tags: tinyPayload.array(appPayload.id),
          content: appPayload.content,
        })
        /* eslint-enable @typescript-eslint/naming-convention */
      ),
    }),
  }
)

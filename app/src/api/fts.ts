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
        slug: appPayload.checkSlug,
        tags: payload.checkArray(appPayload.checkId),
        content: appPayload.checkContent,
      })
      /* eslint-enable @typescript-eslint/naming-convention */
    ),
  })
)

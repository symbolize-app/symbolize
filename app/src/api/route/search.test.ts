import * as appFtsTest from '@app/api/fts.test.ts'
import type * as appFts from '@app/api/fts.ts'
import * as appRouteSearch from '@app/api/route/search.ts'
import * as appLanguage from '@app/core/language.ts'
import * as routeTest from '@tiny/api/route.test.ts'
import * as tinyErrorTest from '@tiny/core/error.test.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinyRandom from '@tiny/core/random.ts'
import * as tinySubmitTest from '@tiny/core/submit.test.ts'
import type * as tinySubmit from '@tiny/core/submit.ts'
import * as tinyTest from '@tiny/test/index.ts'
import lodashMapKeys from 'lodash-es/mapKeys.js'
import lodashSnakeCase from 'lodash-es/snakeCase.js'

export const url = import.meta.url

export const tests = {
  ['search query, no error']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const results = [
      {
        content: 'Hi there',
        createdAt: 1637107200000,
        createdBy: tinyRandom.requestIdHex(baseContext),
        id: tinyRandom.requestIdHex(baseContext),
        names: [],
        parents: [],
        slug: 'hello',
        subforumId: tinyRandom.requestIdHex(baseContext),
        tags: [tinyRandom.requestIdHex(baseContext)],
        taxonRank: null,
        title: 'Hello',
        topicId: tinyRandom.requestIdHex(baseContext),
        type: 'topic',
        updatedAt: 1637193600000,
      },
      {
        content: 'Test 3',
        createdAt: 1637200800000,
        createdBy: tinyRandom.requestIdHex(baseContext),
        id: tinyRandom.requestIdHex(baseContext),
        names: [],
        parents: [],
        slug: 'hello',
        subforumId: tinyRandom.requestIdHex(baseContext),
        tags: [tinyRandom.requestIdHex(baseContext)],
        taxonRank: null,
        title: null,
        topicId: tinyRandom.requestIdHex(baseContext),
        type: 'reply',
        updatedAt: 1637200800000,
      },
      {
        content: 'Faces the sun',
        createdAt: 1637193600000,
        createdBy: tinyRandom.requestIdHex(baseContext),
        id: tinyRandom.requestIdHex(baseContext),
        names: ['sunflower'],
        parents: [tinyRandom.requestIdHex(baseContext)],
        slug: 'sunflower',
        subforumId: null,
        tags: [],
        taxonRank: 'species',
        title: null,
        topicId: null,
        type: 'taxon',
        updatedAt: 1637193600000,
      },
      {
        content: 'Check if wet or dry',
        createdAt: 1637107200000,
        createdBy: tinyRandom.requestIdHex(baseContext),
        id: tinyRandom.requestIdHex(baseContext),
        names: [],
        parents: [],
        slug: 'water',
        subforumId: null,
        tags: [tinyRandom.requestIdHex(baseContext)],
        taxonRank: null,
        title: 'Water',
        topicId: null,
        type: 'info',
        updatedAt: 1637193600000,
      },
    ]
    const submit = tinyTest.mock<
      () => Promise<tinySubmit.Response>
    >([
      () =>
        Promise.resolve(
          tinySubmitTest.mockResponse({
            json: () =>
              Promise.resolve({
                results: results.map((result) =>
                  lodashMapKeys(result, (_value, key) =>
                    lodashSnakeCase(key)
                  )
                ),
              }),
          })
        ),
    ])
    const ctx: tinyTest.Context &
      tinyError.Context &
      tinySubmit.Context &
      appFts.Context = {
      ...baseContext,
      submit,
      submitRetryConfig: tinyErrorTest.retryConfig,
      ...appFtsTest.initContext(),
    }
    const response = tinyTest.sync(
      appRouteSearch.query.handler(
        ctx,
        routeTest.mockReqeuest({
          params: {
            language: appLanguage.Language.english,
            query: 'test',
          },
        })
      )
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assertDeepEquals(response.resolvedValue, {
      status: 200,
      headers: { 'content-type': 'application/json' },
      json: {
        results,
      },
    })
    tinyTest.assertDeepEquals(
      submit[tinyTest.mockHistory],
      [
        [
          {
            headers: {
              ['Authorization']: 'Basic OkZUUw==',
            },
            method: 'GET',
            origin: 'https://fts/',
            params: {
              language: 'en',
              query: 'test',
            },
            path: '/query',
          },
        ],
      ]
    )
  },
}

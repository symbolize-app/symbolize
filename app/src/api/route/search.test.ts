import * as appFtsTest from '@fe/api/fts.test.ts'
import type * as appFts from '@fe/api/fts.ts'
import * as appRouteSearch from '@fe/api/route/search.ts'
import * as appLanguage from '@fe/core/language.ts'
import * as routeTest from '@tiny/api/route.test.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as random from '@tiny/core/random.ts'
import * as submitTest from '@tiny/core/submit.test.ts'
import type * as submit from '@tiny/core/submit.ts'
import * as test from '@tiny/test/index.ts'
import mapKeys from 'lodash-es/mapKeys.js'
import snakeCase from 'lodash-es/snakeCase.js'

export const url = import.meta.url

export const tests = {
  ['search query, no error']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const results = [
      {
        content: 'Hi there',
        createdAt: 1637107200000,
        createdBy: random.requestIdHex(baseContext),
        id: random.requestIdHex(baseContext),
        names: [],
        parents: [],
        slug: 'hello',
        subforumId: random.requestIdHex(baseContext),
        tags: [random.requestIdHex(baseContext)],
        taxonRank: null,
        title: 'Hello',
        topicId: random.requestIdHex(baseContext),
        type: 'topic',
        updatedAt: 1637193600000,
      },
      {
        content: 'Test 3',
        createdAt: 1637200800000,
        createdBy: random.requestIdHex(baseContext),
        id: random.requestIdHex(baseContext),
        names: [],
        parents: [],
        slug: 'hello',
        subforumId: random.requestIdHex(baseContext),
        tags: [random.requestIdHex(baseContext)],
        taxonRank: null,
        title: null,
        topicId: random.requestIdHex(baseContext),
        type: 'reply',
        updatedAt: 1637200800000,
      },
      {
        content: 'Faces the sun',
        createdAt: 1637193600000,
        createdBy: random.requestIdHex(baseContext),
        id: random.requestIdHex(baseContext),
        names: ['sunflower'],
        parents: [random.requestIdHex(baseContext)],
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
        createdBy: random.requestIdHex(baseContext),
        id: random.requestIdHex(baseContext),
        names: [],
        parents: [],
        slug: 'water',
        subforumId: null,
        tags: [random.requestIdHex(baseContext)],
        taxonRank: null,
        title: 'Water',
        topicId: null,
        type: 'info',
        updatedAt: 1637193600000,
      },
    ]
    const submit = test.mock<
      () => Promise<submit.Response>
    >([
      () =>
        Promise.resolve(
          submitTest.mockResponse({
            json: () =>
              Promise.resolve({
                results: results.map((result) =>
                  mapKeys(result, (_value, key) =>
                    snakeCase(key)
                  )
                ),
              }),
          })
        ),
    ])
    const ctx: test.Context &
      errorModule.Context &
      submit.Context &
      appFts.Context = {
      ...baseContext,
      submit,
      ...appFtsTest.initContext(),
    }
    const response = test.sync(
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
    test.assertDeepEquals(response.resolvedValue, {
      status: 200,
      headers: { 'content-type': 'application/json' },
      json: {
        results,
      },
    })
    test.assertDeepEquals(submit[test.mockHistory], [
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
    ])
  },
}

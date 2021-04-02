import type * as apiContext from '@fe/api/context.ts'
import * as apiQuery from '@fe/api/query.ts'
import type * as db from '@fe/db/index.ts'
import * as memberQuery from '@fe/db/query/member.ts'
import * as requestTest from '@tiny/api/route.test.ts'
import * as test from '@tiny/test/index.ts'
import type * as errorModule from '@tiny/util/error.ts'

export const url = import.meta.url

export const tests = {
  async ['member create, no error'](
    baseContext: test.TestContext
  ): Promise<void> {
    const query = test.mock([
      () => Promise.resolve({ rows: [] }),
    ])
    const ctx: test.TestContext &
      errorModule.RetryContext &
      apiContext.DatabaseWriteContext = {
      ...baseContext,
      random: test.mock([]),
      databaseApiWrite: {
        pool: {
          query,
        } as unknown,
      } as db.DatabaseApiWrite,
    }
    const expectedId =
      '9606e52f00c679e548b5155af5026f5af4130d7a15c990a791fff8d652c464f5'
    const response = test.sync(
      apiQuery.apiMemberCreate.handler(
        ctx,
        requestTest.mockReqeuest({
          json: () =>
            Promise.resolve({
              requestId: 'ABCD',
              email: 'test@example.org',
              handle: 'test',
            }),
        })
      )
    )
    await ctx.clock.tickAsync(0)
    test.assertDeepEquals(response.resolvedValue, {
      status: 200,
      headers: { 'content-type': 'application/json' },
      body: {
        id: expectedId,
      },
    })
    test.assertDeepEquals(query[test.mockHistory], [
      [
        {
          name: memberQuery.create.queryName,
          text: memberQuery.create.queryText,
        },
        [
          Buffer.from(expectedId, 'hex'),
          'test@example.org',
          'test',
        ],
      ],
    ])
  },
}

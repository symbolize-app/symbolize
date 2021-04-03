import * as apiQuery from '@fe/api/query.ts'
import type * as db from '@fe/db/index.ts'
import * as memberQuery from '@fe/db/query/member.ts'
import * as requestTest from '@tiny/api/route.test.ts'
import * as queryTest from '@tiny/db/query.test.ts'
import * as test from '@tiny/test/index.ts'
import type * as errorModule from '@tiny/util/error.ts'
import * as random from '@tiny/util/random.ts'

export const url = import.meta.url

export const tests = {
  ['member create, no error']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const query = test.mock([
      () => Promise.resolve({ rows: [] }),
    ])
    const ctx: test.Context &
      errorModule.Context &
      db.WriteContext = {
      ...baseContext,
      databaseApiWrite: {
        pool: {
          query,
        } as unknown,
      } as db.DatabaseApiWrite,
    }
    const expectedId =
      'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
    const response = test.sync(
      apiQuery.apiMemberCreate.handler(
        ctx,
        requestTest.mockReqeuest({
          json: () =>
            Promise.resolve({
              requestId: random.requestIdHex(ctx),
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
  ['member create, uniqueness error']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const query = test.mock([
      () =>
        Promise.reject(
          new queryTest.MockUniqueViolationConstraintError(
            'member_email_key'
          )
        ),
    ])
    const ctx: test.Context &
      errorModule.Context &
      db.WriteContext = {
      ...baseContext,
      databaseApiWrite: {
        pool: {
          query,
        } as unknown,
      } as db.DatabaseApiWrite,
    }
    const expectedId =
      'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
    const response = test.sync(
      apiQuery.apiMemberCreate.handler(
        ctx,
        requestTest.mockReqeuest({
          json: () =>
            Promise.resolve({
              requestId: random.requestIdHex(ctx),
              email: 'test@example.org',
              handle: 'test',
            }),
        })
      )
    )
    await ctx.clock.tickAsync(0)
    test.assertDeepEquals(response.resolvedValue, {
      status: 409,
      headers: { 'content-type': 'application/json' },
      body: {
        conflict: 'email',
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

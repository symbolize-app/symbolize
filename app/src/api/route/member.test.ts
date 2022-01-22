import * as appRouteMember from '@fe/api/route/member.ts'
import * as appDbQuery from '@fe/db/query/index.ts'
import * as appDbQueryMember from '@fe/db/query/member.ts'
import * as tinyRouteTest from '@tiny/api/route.test.ts'
import * as tinyRoute from '@tiny/api/route.ts'
import * as tinyErrorTest from '@tiny/core/error.test.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinyRandom from '@tiny/core/random.ts'
import * as tinyDbQueryTest from '@tiny/db/query.test.ts'
import type * as tinyDbQuery from '@tiny/db/query.ts'
import * as tinyTest from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['member create, no error']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const queryMethod = tinyTest.mock<
      tinyDbQuery.Database<appDbQuery.Write>['query']
    >([() => Promise.resolve()])
    const ctx: tinyTest.Context &
      tinyError.Context &
      appDbQuery.WriteContext = {
      ...baseContext,
      databases: {
        [appDbQuery.write]: {
          query: queryMethod,
        },
      },
      databaseRetryConfig: tinyErrorTest.retryConfig,
    }
    const expectedId =
      'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
    const response = tinyTest.sync(
      appRouteMember.create.handler(
        ctx,
        tinyRouteTest.mockReqeuest({
          json: () =>
            Promise.resolve({
              requestId: tinyRandom.requestIdHex(ctx),
              email: 'test@example.org',
              handle: 'test',
            }),
        })
      )
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assertDeepEquals(response.resolvedValue, {
      status: 200,
      headers: { 'content-type': 'application/json' },
      json: {
        id: expectedId,
      },
    })
    tinyTest.assertDeepEquals(
      queryMethod[tinyTest.mockHistory],
      [
        [
          appDbQueryMember.create,
          Buffer.from(expectedId, 'hex'),
          'test@example.org',
          'test',
        ],
      ]
    )
  },
  ['member create, uniqueness error']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const queryMethod = tinyTest.mock<
      tinyDbQuery.Database<appDbQuery.Write>['query']
    >([
      () =>
        Promise.reject(
          new tinyDbQueryTest.MockUniqueViolationConstraintError(
            'member_email_key'
          )
        ),
    ])
    const ctx: tinyTest.Context &
      tinyError.Context &
      appDbQuery.WriteContext = {
      ...baseContext,
      databases: {
        [appDbQuery.write]: {
          query: queryMethod,
        },
      },
      databaseRetryConfig: tinyErrorTest.retryConfig,
    }
    const expectedId =
      'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
    const response = tinyTest.sync(
      appRouteMember.create.handler(
        ctx,
        tinyRouteTest.mockReqeuest({
          json: () =>
            Promise.resolve({
              requestId: tinyRandom.requestIdHex(ctx),
              email: 'test@example.org',
              handle: 'test',
            }),
        })
      )
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assert(
      response.rejectedValue instanceof
        tinyRoute.ResponseError
    )
    tinyTest.assertDeepEquals(
      response.rejectedValue.response,
      {
        status: 409,
        headers: { 'content-type': 'application/json' },
        json: {
          conflict: 'email',
        },
      }
    )
    tinyTest.assertDeepEquals(
      queryMethod[tinyTest.mockHistory],
      [
        [
          appDbQueryMember.create,
          Buffer.from(expectedId, 'hex'),
          'test@example.org',
          'test',
        ],
      ]
    )
  },
  ['member create, bad request']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const ctx: tinyTest.Context &
      tinyError.Context &
      appDbQuery.WriteContext = {
      ...baseContext,
      databases: {
        [appDbQuery.write]: {
          query: tinyTest.mock<
            tinyDbQuery.Database<appDbQuery.Write>['query']
          >([]),
        },
      },
      databaseRetryConfig: tinyErrorTest.retryConfig,
    }
    const response = tinyTest.sync(
      appRouteMember.create.handler(
        ctx,
        tinyRouteTest.mockReqeuest({
          json: () =>
            Promise.resolve({
              requestId: tinyRandom.requestIdHex(ctx),
              email: 'test@example.org',
              handle: 't',
            }),
        })
      )
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assert(
      response.rejectedValue instanceof
        tinyRoute.ResponseError
    )
    tinyTest.assertDeepEquals(
      response.rejectedValue.response,
      {
        status: 400,
        headers: { 'content-type': 'application/json' },
        json: {
          error:
            'Invalid string (too short, min 3) at (root).handle',
        },
      }
    )
  },
}

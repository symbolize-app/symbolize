import * as appRouteMember from '@fe/api/route/member.ts'
import * as appDbQuery from '@fe/db/query/index.ts'
import * as appDbQueryMember from '@fe/db/query/member.ts'
import * as routeTest from '@tiny/api/route.test.ts'
import * as route from '@tiny/api/route.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as random from '@tiny/core/random.ts'
import * as dbQueryTest from '@tiny/db/query.test.ts'
import type * as dbQuery from '@tiny/db/query.ts'
import * as test from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['member create, no error']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const queryMethod = test.mock<
      dbQuery.Database<appDbQuery.Write>['query']
    >([() => Promise.resolve()])
    const ctx: test.Context &
      errorModule.Context &
      appDbQuery.WriteContext = {
      ...baseContext,
      databases: {
        [appDbQuery.write]: {
          query: queryMethod,
        },
      },
    }
    const expectedId =
      'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
    const response = test.sync(
      appRouteMember.create.handler(
        ctx,
        routeTest.mockReqeuest({
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
      json: {
        id: expectedId,
      },
    })
    test.assertDeepEquals(queryMethod[test.mockHistory], [
      [
        appDbQueryMember.create,
        Buffer.from(expectedId, 'hex'),
        'test@example.org',
        'test',
      ],
    ])
  },
  ['member create, uniqueness error']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const queryMethod = test.mock<
      dbQuery.Database<appDbQuery.Write>['query']
    >([
      () =>
        Promise.reject(
          new dbQueryTest.MockUniqueViolationConstraintError(
            'member_email_key'
          )
        ),
    ])
    const ctx: test.Context &
      errorModule.Context &
      appDbQuery.WriteContext = {
      ...baseContext,
      databases: {
        [appDbQuery.write]: {
          query: queryMethod,
        },
      },
    }
    const expectedId =
      'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
    const response = test.sync(
      appRouteMember.create.handler(
        ctx,
        routeTest.mockReqeuest({
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
    test.assert(
      response.rejectedValue instanceof route.ResponseError
    )
    test.assertDeepEquals(response.rejectedValue.response, {
      status: 409,
      headers: { 'content-type': 'application/json' },
      json: {
        conflict: 'email',
      },
    })
    test.assertDeepEquals(queryMethod[test.mockHistory], [
      [
        appDbQueryMember.create,
        Buffer.from(expectedId, 'hex'),
        'test@example.org',
        'test',
      ],
    ])
  },
  ['member create, bad request']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const ctx: test.Context &
      errorModule.Context &
      appDbQuery.WriteContext = {
      ...baseContext,
      databases: {
        [appDbQuery.write]: {
          query: test.mock<
            dbQuery.Database<appDbQuery.Write>['query']
          >([]),
        },
      },
    }
    const response = test.sync(
      appRouteMember.create.handler(
        ctx,
        routeTest.mockReqeuest({
          json: () =>
            Promise.resolve({
              requestId: random.requestIdHex(ctx),
              email: 'test@example.org',
              handle: 't',
            }),
        })
      )
    )
    await ctx.clock.tickAsync(0)
    test.assert(
      response.rejectedValue instanceof route.ResponseError
    )
    test.assertDeepEquals(response.rejectedValue.response, {
      status: 400,
      headers: { 'content-type': 'application/json' },
      json: {
        error:
          'Invalid string (too short, min 3) at (root).handle',
      },
    })
  },
}

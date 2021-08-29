import * as appRouteMember from '@fe/api/route/member.ts'
import type * as appQuery from '@fe/db/query/index.ts'
import * as appQueryMember from '@fe/db/query/member.ts'
import * as routeTest from '@tiny/api/route.test.ts'
import * as route from '@tiny/api/route.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as random from '@tiny/core/random.ts'
import * as queryTest from '@tiny/db/query.test.ts'
import * as query from '@tiny/db/query.ts'
import * as test from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['member create, no error']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const queryMethod = test.mock<
      query.Database<appQuery.Write>['query']
    >([() => Promise.resolve()])
    const ctx: test.Context &
      errorModule.Context &
      appQuery.WriteContext = {
      ...baseContext,
      databaseApiWrite: query.createDatabase({
        query: queryMethod,
      }),
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
      body: {
        id: expectedId,
      },
    })
    test.assertDeepEquals(queryMethod[test.mockHistory], [
      [
        appQueryMember.create,
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
      query.Database<appQuery.Write>['query']
    >([
      () =>
        Promise.reject(
          new queryTest.MockUniqueViolationConstraintError(
            'member_email_key'
          )
        ),
    ])
    const ctx: test.Context &
      errorModule.Context &
      appQuery.WriteContext = {
      ...baseContext,
      databaseApiWrite: query.createDatabase({
        query: queryMethod,
      }),
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
      body: {
        conflict: 'email',
      },
    })
    test.assertDeepEquals(queryMethod[test.mockHistory], [
      [
        appQueryMember.create,
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
      appQuery.WriteContext = {
      ...baseContext,
      databaseApiWrite: query.createDatabase({
        query: test.mock<
          query.Database<appQuery.Write>['query']
        >([]),
      }),
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
      body: {
        error:
          'Invalid string (too short, min 3) at (root).handle',
      },
    })
  },
}

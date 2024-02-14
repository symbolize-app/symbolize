import * as error from '@/index.ts'
import type * as random from '@intertwine/lib-random'
import * as test from '@intertwine/lib-test'
import * as time from '@intertwine/lib-time'

export const retryConfig: Omit<error.RetryConfig, 'onError'> = {
  maxAttempts: 0,
  minDelayMs: 0,
  windowMs: 0,
}

export const url = import.meta.url

export const tests = {
  async ['one attempt, pass'](baseContext: test.Context): Promise<void> {
    const ctx: error.Context & test.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock<random.Random['number']>([]),
      },
    }
    const expectedResult = {}
    const onError = test.mock([])
    const actualResult = test.sync(
      error.retry(ctx, async () => Promise.resolve(expectedResult), {
        maxAttempts: 10,
        minDelayMs: time.interval({
          milliseconds: 10,
        }),
        onError,
        windowMs: time.interval({ seconds: 10 }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertEquals(actualResult.resolvedValue, expectedResult)
  },

  async ['two attempts, count limit'](
    baseContext: test.Context,
  ): Promise<void> {
    const ctx: error.Context & test.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock<random.Random['number']>([() => 0.5]),
      },
    }
    const expectedResult = new Error('TEST')
    const [onError, onErrorHistory] = test.mockWithHistory<
      error.RetryConfig['onError']
    >([() => undefined])
    const actualResult = test.sync(
      error.retry(ctx, async () => Promise.reject(expectedResult), {
        maxAttempts: 2,
        minDelayMs: time.interval({
          milliseconds: 10,
        }),
        onError,
        windowMs: time.interval({ seconds: 10 }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onErrorHistory, [[expectedResult, 0, 10]])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(10)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },

  async ['three attempts, count limit'](
    baseContext: test.Context,
  ): Promise<void> {
    const ctx: error.Context & test.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock<random.Random['number']>([() => 0.8, () => 0.8]),
      },
    }
    const expectedResult = new Error('TEST')
    const [onError, onErrorHistory] = test.mockWithHistory<
      error.RetryConfig['onError']
    >([() => undefined, () => undefined])
    const actualResult = test.sync(
      error.retry(ctx, async () => Promise.reject(expectedResult), {
        maxAttempts: 3,
        minDelayMs: time.interval({
          milliseconds: 10,
        }),
        onError,
        windowMs: time.interval({ seconds: 10 }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onErrorHistory, [[expectedResult, 0, 10]])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(10)
    test.assertDeepEquals(onErrorHistory, [
      [expectedResult, 0, 10],
      [expectedResult, 1, 16],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(16)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },

  async ['one attempt, window limit'](
    baseContext: test.Context,
  ): Promise<void> {
    const ctx: error.Context & test.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock<random.Random['number']>([() => 0]),
      },
    }
    const expectedResult = new Error('TEST')
    const onError = test.mock([])
    const actualResult = test.sync(
      error.retry(
        ctx,
        async () => {
          await time.delay(ctx, time.interval({ seconds: 9 }))
          throw expectedResult
        },
        {
          maxAttempts: 10,
          minDelayMs: time.interval({ seconds: 1 }),
          onError,
          windowMs: time.interval({ seconds: 10 }),
        },
      ),
    )
    await ctx.time.clock.tickAsync(0)
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(9_000)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },

  async ['three attempts, window limit'](
    baseContext: test.Context,
  ): Promise<void> {
    const ctx: error.Context & test.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock<random.Random['number']>([
          () => 0.6,
          () => 0.6,
          () => 0,
        ]),
      },
    }
    const expectedResult = new Error('TEST')
    const [onError, onErrorHistory] = test.mockWithHistory<
      error.RetryConfig['onError']
    >([() => undefined, () => undefined])
    const actualResult = test.sync(
      error.retry(ctx, async () => Promise.reject(expectedResult), {
        maxAttempts: 10,
        minDelayMs: time.interval({ seconds: 1 }),
        onError,
        windowMs: time.interval({
          milliseconds: 201,
          seconds: 2,
        }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onErrorHistory, [[expectedResult, 0, 1000]])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(1000)
    test.assertDeepEquals(onErrorHistory, [
      [expectedResult, 0, 1000],
      [expectedResult, 1, 1200],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(1200)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },

  async ['three attempts, pass'](
    baseContext: test.Context,
  ): Promise<void> {
    const ctx: error.Context & test.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock<random.Random['number']>([() => 0.8, () => 0.8]),
      },
    }
    const expectedResult = {}
    const expectedError = new Error('TEST')
    const callback = test.mock([
      async () => Promise.reject(expectedError),
      async () => Promise.reject(expectedError),
      async () => Promise.resolve(expectedResult),
    ])
    const [onError, onErrorHistory] = test.mockWithHistory<
      error.RetryConfig['onError']
    >([() => undefined, () => undefined])
    const actualResult = test.sync(
      error.retry(ctx, callback, {
        maxAttempts: 3,
        minDelayMs: time.interval({ milliseconds: 10 }),
        onError,
        windowMs: time.interval({ seconds: 10 }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onErrorHistory, [[expectedError, 0, 10]])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(10)
    test.assertDeepEquals(onErrorHistory, [
      [expectedError, 0, 10],
      [expectedError, 1, 16],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(16)
    test.assertEquals(actualResult.resolvedValue, expectedResult)
  },
}

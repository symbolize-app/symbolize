import * as error from '@/index.ts'
import type * as random from '@intertwine/lib-random'
import * as test from '@intertwine/lib-test'
import * as time from '@intertwine/lib-time'
import type * as timeTest from '@intertwine/lib-time/test.ts'

export const retryConfig: Omit<error.RetryConfig, 'onError' | 'signal'> = {
  maxAttempts: 0,
  minDelayMs: 0,
  windowMs: 0,
}

export const url = import.meta.url

export const tests = {
  async ['one attempt, pass'](
    baseContext: random.Context & timeTest.Context,
  ): Promise<void> {
    const ctx = {
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
        signal: null,
        windowMs: time.interval({ seconds: 10 }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertEquals(actualResult.resolvedValue, expectedResult)
  },

  async ['two attempts, count limit'](
    baseContext: random.Context & timeTest.Context,
  ): Promise<void> {
    const ctx = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock<random.Random['number']>([() => 0.7]),
      },
    }
    const expectedResult = new Error('TEST')
    const [onError, onErrorHistory] = test.mockWithHistory<
      Exclude<error.RetryConfig['onError'], null>
    >([() => undefined])
    const actualResult = test.sync(
      error.retry(ctx, async () => Promise.reject(expectedResult), {
        maxAttempts: 2,
        minDelayMs: time.interval({
          milliseconds: 10,
        }),
        onError,
        signal: null,
        windowMs: time.interval({ seconds: 10 }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onErrorHistory, [[expectedResult, 0, 17]])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(17)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },

  async ['three attempts, count limit'](
    baseContext: random.Context & timeTest.Context,
  ): Promise<void> {
    const ctx = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock<random.Random['number']>([() => 0.8, () => 0.8]),
      },
    }
    const expectedResult = new Error('TEST')
    const [onError, onErrorHistory] = test.mockWithHistory<
      Exclude<error.RetryConfig['onError'], null>
    >([() => undefined, () => undefined])
    const actualResult = test.sync(
      error.retry(ctx, async () => Promise.reject(expectedResult), {
        maxAttempts: 3,
        minDelayMs: time.interval({
          milliseconds: 10,
        }),
        onError,
        signal: null,
        windowMs: time.interval({ seconds: 10 }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onErrorHistory, [[expectedResult, 0, 18]])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(18)
    test.assertDeepEquals(onErrorHistory, [
      [expectedResult, 0, 18],
      [expectedResult, 1, 26],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(26)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },

  async ['one attempt, window limit'](
    baseContext: random.Context & timeTest.Context,
  ): Promise<void> {
    const ctx = {
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
          signal: null,
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
    baseContext: random.Context & timeTest.Context,
  ): Promise<void> {
    const ctx = {
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
      Exclude<error.RetryConfig['onError'], null>
    >([() => undefined, () => undefined])
    const actualResult = test.sync(
      error.retry(ctx, async () => Promise.reject(expectedResult), {
        maxAttempts: 10,
        minDelayMs: time.interval({ seconds: 1 }),
        onError,
        signal: null,
        windowMs: time.interval({
          milliseconds: 801,
          seconds: 3,
        }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onErrorHistory, [[expectedResult, 0, 1600]])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(1600)
    test.assertDeepEquals(onErrorHistory, [
      [expectedResult, 0, 1600],
      [expectedResult, 1, 2200],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(2200)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },

  async ['three attempts, pass'](
    baseContext: random.Context & timeTest.Context,
  ): Promise<void> {
    const ctx = {
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
      Exclude<error.RetryConfig['onError'], null>
    >([() => undefined, () => undefined])
    const actualResult = test.sync(
      error.retry(ctx, callback, {
        maxAttempts: 3,
        minDelayMs: time.interval({ milliseconds: 10 }),
        onError,
        signal: null,
        windowMs: time.interval({ seconds: 10 }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onErrorHistory, [[expectedError, 0, 18]])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(18)
    test.assertDeepEquals(onErrorHistory, [
      [expectedError, 0, 18],
      [expectedError, 1, 26],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(26)
    test.assertEquals(actualResult.resolvedValue, expectedResult)
  },

  async ['one attempt, abort'](
    baseContext: random.Context & timeTest.Context,
  ): Promise<void> {
    const ctx = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock<random.Random['number']>([() => 0.5]),
      },
    }
    const expectedResult = new Error('TEST')
    const [onError, onErrorHistory] = test.mockWithHistory<
      Exclude<error.RetryConfig['onError'], null>
    >([() => undefined])
    const abortController = new AbortController()
    const actualResult = test.sync(
      error.retry(ctx, async () => Promise.reject(expectedResult), {
        maxAttempts: 10,
        minDelayMs: time.interval({ milliseconds: 100 }),
        onError,
        signal: abortController.signal,
        windowMs: time.interval({ seconds: 10 }),
      }),
    )
    await ctx.time.clock.tickAsync(0)
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(100)
    test.assertDeepEquals(onErrorHistory, [[expectedResult, 0, 150]])
    test.assert(!actualResult.isSettled)
    abortController.abort()
    await ctx.time.clock.tickAsync(0)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },
}

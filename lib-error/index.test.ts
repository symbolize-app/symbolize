import * as error from '@/index.ts'
import * as test from '@intertwine/lib-test'
import * as time from '@intertwine/lib-time'

export const retryConfig: Omit<error.RetryConfig, 'onError'> = {
  maxAttempts: 0,
  minDelayMs: 0,
  windowMs: 0,
}

export const url = import.meta.url

export const tests = {
  ['one attempt, pass']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const ctx: test.Context & error.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock([]),
      },
    }
    const expectedResult = {}
    const onError = test.mock([])
    const actualResult = test.sync(
      error.retry(ctx, () => Promise.resolve(expectedResult), {
        minDelayMs: time.interval({
          milliseconds: 10,
        }),
        windowMs: time.interval({ seconds: 10 }),
        maxAttempts: 10,
        onError,
      })
    )
    await ctx.time.clock.tickAsync(0)
    test.assertEquals(actualResult.resolvedValue, expectedResult)
  },
  ['two attempts, count limit']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const ctx: test.Context & error.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock([() => 0.5]),
      },
    }
    const expectedResult = new Error('TEST')
    const onError = test.mock([() => undefined])
    const actualResult = test.sync(
      error.retry(ctx, () => Promise.reject(expectedResult), {
        minDelayMs: time.interval({
          milliseconds: 10,
        }),
        windowMs: time.interval({ seconds: 10 }),
        maxAttempts: 2,
        onError,
      })
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedResult, 0, 10],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(10)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },
  ['three attempts, count limit']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const ctx: test.Context & error.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock([() => 0.8, () => 0.8]),
      },
    }
    const expectedResult = new Error('TEST')
    const onError = test.mock([() => undefined, () => undefined])
    const actualResult = test.sync(
      error.retry(ctx, () => Promise.reject(expectedResult), {
        minDelayMs: time.interval({
          milliseconds: 10,
        }),
        windowMs: time.interval({ seconds: 10 }),
        maxAttempts: 3,
        onError,
      })
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedResult, 0, 10],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(10)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedResult, 0, 10],
      [expectedResult, 1, 16],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(16)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },
  ['one attempt, window limit']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const ctx: test.Context & error.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock([() => 0]),
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
          minDelayMs: time.interval({ seconds: 1 }),
          windowMs: time.interval({ seconds: 10 }),
          maxAttempts: 10,
          onError,
        }
      )
    )
    await ctx.time.clock.tickAsync(0)
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(9_000)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },
  ['three attempts, window limit']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const ctx: test.Context & error.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock([() => 0.6, () => 0.6, () => 0]),
      },
    }
    const expectedResult = new Error('TEST')
    const onError = test.mock([() => undefined, () => undefined])
    const actualResult = test.sync(
      error.retry(ctx, () => Promise.reject(expectedResult), {
        minDelayMs: time.interval({ seconds: 1 }),
        windowMs: time.interval({
          seconds: 2,
          milliseconds: 201,
        }),
        maxAttempts: 10,
        onError,
      })
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedResult, 0, 1000],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(1000)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedResult, 0, 1000],
      [expectedResult, 1, 1200],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(1200)
    test.assertEquals(actualResult.rejectedValue, expectedResult)
  },
  ['three attempts, pass']: async (
    baseContext: test.Context
  ): Promise<void> => {
    const ctx: test.Context & error.Context = {
      ...baseContext,
      random: {
        ...baseContext.random,
        number: test.mock([() => 0.8, () => 0.8]),
      },
    }
    const expectedResult = {}
    const expectedError = new Error('TEST')
    const callback = test.mock([
      () => Promise.reject(expectedError),
      () => Promise.reject(expectedError),
      () => Promise.resolve(expectedResult),
    ])
    const onError = test.mock([() => undefined, () => undefined])
    const actualResult = test.sync(
      error.retry(ctx, callback, {
        minDelayMs: time.interval({ milliseconds: 10 }),
        windowMs: time.interval({ seconds: 10 }),
        maxAttempts: 3,
        onError,
      })
    )
    await ctx.time.clock.tickAsync(0)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedError, 0, 10],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(10)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedError, 0, 10],
      [expectedError, 1, 16],
    ])
    test.assert(!actualResult.isSettled)
    await ctx.time.clock.tickAsync(16)
    test.assertEquals(actualResult.resolvedValue, expectedResult)
  },
}

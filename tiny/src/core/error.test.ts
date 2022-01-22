import * as tinyError from '@tiny/core/error.ts'
import * as tinyTime from '@tiny/core/time.ts'
import * as tinyTest from '@tiny/test/index.ts'

export const retryConfig: Omit<
  tinyError.RetryConfig,
  'onError'
> = {
  maxAttempts: 0,
  minDelayMs: 0,
  windowMs: 0,
}

export const url = import.meta.url

export const tests = {
  ['one attempt, pass']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const ctx: tinyTest.Context & tinyError.Context = {
      ...baseContext,
      randomNumber: tinyTest.mock([]),
    }
    const expectedResult = {}
    const onError = tinyTest.mock([])
    const actualResult = tinyTest.sync(
      tinyError.retry(
        ctx,
        () => Promise.resolve(expectedResult),
        {
          minDelayMs: tinyTime.interval({
            milliseconds: 10,
          }),
          windowMs: tinyTime.interval({ seconds: 10 }),
          maxAttempts: 10,
          onError,
        }
      )
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assertEquals(
      actualResult.resolvedValue,
      expectedResult
    )
  },
  ['two attempts, count limit']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const ctx: tinyTest.Context & tinyError.Context = {
      ...baseContext,
      randomNumber: tinyTest.mock([() => 0.5]),
    }
    const expectedResult = new Error('TEST')
    const onError = tinyTest.mock([() => undefined])
    const actualResult = tinyTest.sync(
      tinyError.retry(
        ctx,
        () => Promise.reject(expectedResult),
        {
          minDelayMs: tinyTime.interval({
            milliseconds: 10,
          }),
          windowMs: tinyTime.interval({ seconds: 10 }),
          maxAttempts: 2,
          onError,
        }
      )
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assertDeepEquals(
      onError[tinyTest.mockHistory],
      [[expectedResult, 0, 10]]
    )
    tinyTest.assert(!actualResult.isSettled)
    await ctx.clock.tickAsync(10)
    tinyTest.assertEquals(
      actualResult.rejectedValue,
      expectedResult
    )
  },
  ['three attempts, count limit']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const ctx: tinyTest.Context & tinyError.Context = {
      ...baseContext,
      randomNumber: tinyTest.mock([() => 0.8, () => 0.8]),
    }
    const expectedResult = new Error('TEST')
    const onError = tinyTest.mock([
      () => undefined,
      () => undefined,
    ])
    const actualResult = tinyTest.sync(
      tinyError.retry(
        ctx,
        () => Promise.reject(expectedResult),
        {
          minDelayMs: tinyTime.interval({
            milliseconds: 10,
          }),
          windowMs: tinyTime.interval({ seconds: 10 }),
          maxAttempts: 3,
          onError,
        }
      )
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assertDeepEquals(
      onError[tinyTest.mockHistory],
      [[expectedResult, 0, 10]]
    )
    tinyTest.assert(!actualResult.isSettled)
    await ctx.clock.tickAsync(10)
    tinyTest.assertDeepEquals(
      onError[tinyTest.mockHistory],
      [
        [expectedResult, 0, 10],
        [expectedResult, 1, 16],
      ]
    )
    tinyTest.assert(!actualResult.isSettled)
    await ctx.clock.tickAsync(16)
    tinyTest.assertEquals(
      actualResult.rejectedValue,
      expectedResult
    )
  },
  ['one attempt, window limit']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const ctx: tinyTest.Context & tinyError.Context = {
      ...baseContext,
      randomNumber: tinyTest.mock([() => 0]),
    }
    const expectedResult = new Error('TEST')
    const onError = tinyTest.mock([])
    const actualResult = tinyTest.sync(
      tinyError.retry(
        ctx,
        async () => {
          await tinyTime.delay(
            ctx,
            tinyTime.interval({ seconds: 9 })
          )
          throw expectedResult
        },
        {
          minDelayMs: tinyTime.interval({ seconds: 1 }),
          windowMs: tinyTime.interval({ seconds: 10 }),
          maxAttempts: 10,
          onError,
        }
      )
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assert(!actualResult.isSettled)
    await ctx.clock.tickAsync(9_000)
    tinyTest.assertEquals(
      actualResult.rejectedValue,
      expectedResult
    )
  },
  ['three attempts, window limit']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const ctx: tinyTest.Context & tinyError.Context = {
      ...baseContext,
      randomNumber: tinyTest.mock([
        () => 0.6,
        () => 0.6,
        () => 0,
      ]),
    }
    const expectedResult = new Error('TEST')
    const onError = tinyTest.mock([
      () => undefined,
      () => undefined,
    ])
    const actualResult = tinyTest.sync(
      tinyError.retry(
        ctx,
        () => Promise.reject(expectedResult),
        {
          minDelayMs: tinyTime.interval({ seconds: 1 }),
          windowMs: tinyTime.interval({
            seconds: 2,
            milliseconds: 201,
          }),
          maxAttempts: 10,
          onError,
        }
      )
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assertDeepEquals(
      onError[tinyTest.mockHistory],
      [[expectedResult, 0, 1000]]
    )
    tinyTest.assert(!actualResult.isSettled)
    await ctx.clock.tickAsync(1000)
    tinyTest.assertDeepEquals(
      onError[tinyTest.mockHistory],
      [
        [expectedResult, 0, 1000],
        [expectedResult, 1, 1200],
      ]
    )
    tinyTest.assert(!actualResult.isSettled)
    await ctx.clock.tickAsync(1200)
    tinyTest.assertEquals(
      actualResult.rejectedValue,
      expectedResult
    )
  },
  ['three attempts, pass']: async (
    baseContext: tinyTest.Context
  ): Promise<void> => {
    const ctx: tinyTest.Context & tinyError.Context = {
      ...baseContext,
      randomNumber: tinyTest.mock([() => 0.8, () => 0.8]),
    }
    const expectedResult = {}
    const expectedError = new Error('TEST')
    const callback = tinyTest.mock([
      () => Promise.reject(expectedError),
      () => Promise.reject(expectedError),
      () => Promise.resolve(expectedResult),
    ])
    const onError = tinyTest.mock([
      () => undefined,
      () => undefined,
    ])
    const actualResult = tinyTest.sync(
      tinyError.retry(ctx, callback, {
        minDelayMs: tinyTime.interval({ milliseconds: 10 }),
        windowMs: tinyTime.interval({ seconds: 10 }),
        maxAttempts: 3,
        onError,
      })
    )
    await ctx.clock.tickAsync(0)
    tinyTest.assertDeepEquals(
      onError[tinyTest.mockHistory],
      [[expectedError, 0, 10]]
    )
    tinyTest.assert(!actualResult.isSettled)
    await ctx.clock.tickAsync(10)
    tinyTest.assertDeepEquals(
      onError[tinyTest.mockHistory],
      [
        [expectedError, 0, 10],
        [expectedError, 1, 16],
      ]
    )
    tinyTest.assert(!actualResult.isSettled)
    await ctx.clock.tickAsync(16)
    tinyTest.assertEquals(
      actualResult.resolvedValue,
      expectedResult
    )
  },
}

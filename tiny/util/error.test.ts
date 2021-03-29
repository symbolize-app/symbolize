import * as test from '@tiny/test/index.ts'
import * as errorModule from '@tiny/util/error.ts'
import * as time from '@tiny/util/time.ts'

export const url = import.meta.url

export const tests = {
  async ['no retries'](
    baseContext: test.TestContext
  ): Promise<void> {
    const ctx: test.TestContext &
      errorModule.RetryContext = {
      ...baseContext,
      random: test.mock([]),
    }
    const expectedResult = {}
    const onError = test.mock([])
    const actualResult = test.sync(
      errorModule.retry(
        ctx,
        () => Promise.resolve(expectedResult),
        {
          minDelayMs: time.interval({ milliseconds: 10 }),
          windowMs: time.interval({ seconds: 10 }),
          maxAttempts: 10,
          onError,
        }
      )
    )
    await ctx.clock.tickAsync(0)
    test.assertEquals(
      actualResult.resolvedValue,
      expectedResult
    )
  },
  async ['one retry'](
    baseContext: test.TestContext
  ): Promise<void> {
    const ctx: test.TestContext &
      errorModule.RetryContext = {
      ...baseContext,
      random: test.mock([() => 0.5]),
    }
    const expectedResult = new Error('TEST')
    const onError = test.mock([() => undefined])
    const actualResult = test.sync(
      errorModule.retry(
        ctx,
        () => Promise.reject(expectedResult),
        {
          minDelayMs: time.interval({ milliseconds: 10 }),
          windowMs: time.interval({ seconds: 10 }),
          maxAttempts: 2,
          onError,
        }
      )
    )
    await ctx.clock.tickAsync(0)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedResult, 0, 10],
    ])
    test.assertEquals(actualResult.isSettled, false)
    await ctx.clock.tickAsync(10)
    test.assertEquals(
      actualResult.rejectedValue,
      expectedResult
    )
  },
  async ['two retries'](
    baseContext: test.TestContext
  ): Promise<void> {
    const ctx: test.TestContext &
      errorModule.RetryContext = {
      ...baseContext,
      random: test.mock([() => 0.8, () => 0.8]),
    }
    const expectedResult = new Error('TEST')
    const onError = test.mock([
      () => undefined,
      () => undefined,
    ])
    const actualResult = test.sync(
      errorModule.retry(
        ctx,
        () => Promise.reject(expectedResult),
        {
          minDelayMs: time.interval({ milliseconds: 10 }),
          windowMs: time.interval({ seconds: 10 }),
          maxAttempts: 3,
          onError,
        }
      )
    )
    await ctx.clock.tickAsync(0)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedResult, 0, 10],
    ])
    test.assertEquals(actualResult.isSettled, false)
    await ctx.clock.tickAsync(10)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedResult, 0, 10],
      [expectedResult, 1, 16],
    ])
    test.assertEquals(actualResult.isSettled, false)
    await ctx.clock.tickAsync(16)
    test.assertEquals(
      actualResult.rejectedValue,
      expectedResult
    )
  },
  async ['two retries, then pass'](
    baseContext: test.TestContext
  ): Promise<void> {
    const ctx: test.TestContext &
      errorModule.RetryContext = {
      ...baseContext,
      random: test.mock([() => 0.8, () => 0.8]),
    }
    const expectedResult = {}
    const expectedError = new Error('TEST')
    const callback = test.mock([
      () => Promise.reject(expectedError),
      () => Promise.reject(expectedError),
      () => Promise.resolve(expectedResult),
    ])
    const onError = test.mock([
      () => undefined,
      () => undefined,
    ])
    const actualResult = test.sync(
      errorModule.retry(ctx, callback, {
        minDelayMs: time.interval({ milliseconds: 10 }),
        windowMs: time.interval({ seconds: 10 }),
        maxAttempts: 3,
        onError,
      })
    )
    await ctx.clock.tickAsync(0)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedError, 0, 10],
    ])
    test.assertEquals(actualResult.isSettled, false)
    await ctx.clock.tickAsync(10)
    test.assertDeepEquals(onError[test.mockHistory], [
      [expectedError, 0, 10],
      [expectedError, 1, 16],
    ])
    test.assertEquals(actualResult.isSettled, false)
    await ctx.clock.tickAsync(16)
    test.assertEquals(
      actualResult.resolvedValue,
      expectedResult
    )
  },
}

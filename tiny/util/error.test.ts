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
      random: test.mock([0.5]),
    }
    const expectedResult = new Error('TEST')
    const onError = test.mock([undefined])
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
}

import type fakeTimers from '@sinonjs/fake-timers'
import * as test from '@tiny/test/index.ts'
import * as time from '@tiny/util/time.ts'

export type TimeTestContext = time.TimeContext & {
  clock: fakeTimers.Clock
}

export const url = import.meta.url

export const tests = {
  async ['delay'](ctx: test.TestContext): Promise<void> {
    const result = test.sync(time.delay(ctx, 20))
    await ctx.clock.tickAsync(0)
    test.assertEquals(result.isSettled, false)
    await ctx.clock.tickAsync(20)
    test.assertEquals(result.resolvedValue, undefined)
  },
  ['interval'](): void {
    test.assertEquals(
      time.interval({
        hours: 2,
        minutes: 2,
        seconds: 2,
        milliseconds: 2,
      }),
      2 * 60 * 60 * 1000 + 2 * 60 * 1000 + 2 * 1000 + 2
    )
  },
  ['convert'](): void {
    test.assertEquals(
      time.convert(2 * 60 * 60 * 1000, 'hours'),
      2
    )
    test.assertEquals(
      time.convert(2 * 60 * 60 * 1000, 'minutes'),
      2 * 60
    )
    test.assertEquals(
      time.convert(2 * 60 * 60 * 1000, 'seconds'),
      2 * 60 * 60
    )
  },
  ['subtract'](): void {
    test.assertDeepEquals(
      time.subtract(
        new Date('2021-03-29T05:12:27.331Z'),
        new Date('2021-03-29T05:12:15.276Z')
      ),
      12055
    )
  },
  ['add'](): void {
    test.assertDeepEquals(
      time.add(new Date('2021-03-29T05:12:15.276Z'), 12055),
      new Date('2021-03-29T05:12:27.331Z')
    )
  },
}
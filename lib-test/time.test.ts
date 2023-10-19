import * as tinyTime from '@intertwine/lib-time/time.ts'

import * as tinyTest from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['delay']: async (
    ctx: tinyTest.Context
  ): Promise<void> => {
    const result = tinyTest.sync(tinyTime.delay(ctx, 20))
    await ctx.clock.tickAsync(0)
    tinyTest.assert(!result.isSettled)
    await ctx.clock.tickAsync(20)
    tinyTest.assertEquals(result.resolvedValue, undefined)
  },
  ['interval']: (): void => {
    tinyTest.assertEquals(
      tinyTime.interval({
        hours: 2,
        minutes: 2,
        seconds: 2,
        milliseconds: 2,
      }),
      2 * 60 * 60 * 1000 + 2 * 60 * 1000 + 2 * 1000 + 2
    )
  },
  ['convert']: (): void => {
    tinyTest.assertEquals(
      tinyTime.convert(2 * 60 * 60 * 1000, 'hours'),
      2
    )
    tinyTest.assertEquals(
      tinyTime.convert(2 * 60 * 60 * 1000, 'minutes'),
      2 * 60
    )
    tinyTest.assertEquals(
      tinyTime.convert(2 * 60 * 60 * 1000, 'seconds'),
      2 * 60 * 60
    )
  },
  ['subtract']: (): void => {
    tinyTest.assertDeepEquals(
      tinyTime.subtract(
        new Date('2021-03-29T05:12:27.331Z'),
        new Date('2021-03-29T05:12:15.276Z')
      ),
      12055
    )
  },
  ['add']: (): void => {
    tinyTest.assertDeepEquals(
      tinyTime.add(
        new Date('2021-03-29T05:12:15.276Z'),
        12055
      ),
      new Date('2021-03-29T05:12:27.331Z')
    )
  },
}

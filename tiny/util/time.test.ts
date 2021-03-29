import type fakeTimers from '@sinonjs/fake-timers'
import type * as time from '@tiny/util/time.ts'

export type TimeTestContext = time.TimeContext & {
  clock: fakeTimers.Clock
}

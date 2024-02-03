import type * as time from '@intertwine/lib-time'
import fakeTimers from '@sinonjs/fake-timers'

export type Context = time.Context & {
  time: {
    clock: fakeTimers.Clock
  }
}

export function initContext(): Context {
  const clock = fakeTimers.createClock(1616952581493)
  return {
    time: {
      clock,
      performanceNow() {
        return clock.now
      },
      setTimeout(callback, ms) {
        return clock.setTimeout(callback, ms)
      },
    },
  }
}

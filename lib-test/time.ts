import type * as time from '@intertwine/lib-time'
import fakeTimers from '@sinonjs/fake-timers'

export type Context = time.Context & {
  clock: fakeTimers.Clock
}

export function initContext(): Context {
  const clock = fakeTimers.createClock(1616952581493)
  return {
    performanceNow: () => clock.now,
    setTimeout: (...args) => clock.setTimeout(...args),
    clock,
  }
}

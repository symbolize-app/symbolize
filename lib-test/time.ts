import type * as tinyTime from '@intertwine/lib-time/time.ts'
import fakeTimers from '@sinonjs/fake-timers'

export type Context = tinyTime.Context & {
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

import type * as time from '@intertwine/lib-time'
import fakeTimers from '@sinonjs/fake-timers'

export type Context = time.Context & {
  time: TimeImpl
}

export class TimeImpl implements time.Time {
  private constructor(readonly clock: Readonly<fakeTimers.Clock>) {}

  static build(): TimeImpl {
    const clock = fakeTimers.createClock(1616952581493)
    return new TimeImpl(clock)
  }

  performanceNow(): number {
    return this.clock.now
  }

  setTimeout(callback: () => void, ms: number): unknown {
    return this.clock.setTimeout(callback, ms)
  }
}

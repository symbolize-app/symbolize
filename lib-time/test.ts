import type * as time_ from '@/index.ts'
import fakeTimers from '@sinonjs/fake-timers'

export interface Context {
  readonly time: Time
}

export interface Time extends time_.Time {
  readonly clock: Readonly<fakeTimers.Clock>
}

class TimeImpl implements Time {
  constructor(readonly clock: Readonly<fakeTimers.Clock>) {}

  performanceNow(): number {
    return this.clock.now
  }

  setTimeout(callback: () => void, ms: number): unknown {
    return this.clock.setTimeout(callback, ms)
  }
}

export function time(): Time {
  const clock = fakeTimers.createClock(1616952581493)
  return new TimeImpl(clock)
}

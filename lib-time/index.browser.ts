import type * as time_ from '@/index.ts'

class TimeImpl implements time_.Time {
  performanceNow(): number {
    return globalThis.performance.now()
  }

  setTimeout(callback: () => void, ms: number): unknown {
    return globalThis.setTimeout(callback, ms)
  }
}

export function time(): time_.Time {
  return new TimeImpl()
}

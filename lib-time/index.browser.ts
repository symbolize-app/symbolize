import type * as time from '@/index.ts'

export class TimeImpl implements time.Time {
  performanceNow(): number {
    return globalThis.performance.now()
  }

  setTimeout(callback: () => void, ms: number): unknown {
    return globalThis.setTimeout(callback, ms)
  }
}

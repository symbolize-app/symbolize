import type * as time_ from '@/index.ts'
import * as nodePerfHooks from 'node:perf_hooks'

class TimeImpl implements time_.Time {
  performanceNow(): number {
    return nodePerfHooks.performance.now()
  }

  setTimeout(callback: () => void, ms: number): unknown {
    return global.setTimeout(callback, ms)
  }
}

export function time(): time_.Time {
  return new TimeImpl()
}

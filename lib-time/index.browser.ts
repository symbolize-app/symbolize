import type * as time from '@intertwine/lib-time'

export function initContext(): time.Context {
  return {
    time: {
      performanceNow() {
        return globalThis.performance.now()
      },
      setTimeout(callback, ms) {
        return globalThis.setTimeout(callback, ms)
      },
    },
  }
}

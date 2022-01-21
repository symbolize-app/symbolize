import type * as errorModule from '@tiny/core/error.ts'
import * as time from '@tiny/core/time.ts'

export const retryConfig: Omit<
  errorModule.RetryConfig,
  'onError'
> = {
  maxAttempts: 15,
  minDelayMs: time.interval({
    milliseconds: 10,
  }),
  windowMs: time.interval({ seconds: 30 }),
}

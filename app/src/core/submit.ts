import type * as tinyError from '@tiny/core/error.ts'
import * as tinyTime from '@tiny/core/time.ts'

export const retryConfig: Omit<
  tinyError.RetryConfig,
  'onError'
> = {
  maxAttempts: 15,
  minDelayMs: tinyTime.interval({
    milliseconds: 10,
  }),
  windowMs: tinyTime.interval({ seconds: 30 }),
}

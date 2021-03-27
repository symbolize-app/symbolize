import * as time from '@tiny/util/time.ts'
import type * as typeFest from 'type-fest'

export type RetryContext = { now: () => number }

export type RetryConfig = {
  minDelayMs: number
  windowMs: number
  maxAttempts: number
  onError: (
    error: unknown,
    attempt: number,
    nextDelayMs: number
  ) => NextRetryAction
}

export enum NextRetryAction {
  retry,
  stop,
}

export async function retry<Result>(
  ctx: RetryContext,
  callback: () => typeFest.Promisable<Result>,
  config: RetryConfig
): Promise<Result> {
  const startMs = ctx.now()
  let attempt = 0

  while (true) {
    try {
      return await callback()
    } catch (error: unknown) {
      if (attempt === config.maxAttempts - 1) {
        throw error
      }

      const nowMs = ctx.now()
      const delayMs = Math.max(
        config.minDelayMs,
        Math.random() *
          config.minDelayMs *
          Math.pow(2, attempt)
      )
      if (nowMs + delayMs > startMs + config.windowMs) {
        throw error
      }

      const nextAction = config.onError(
        error,
        attempt,
        delayMs
      )
      if (nextAction === NextRetryAction.stop) {
        throw error
      }

      await time.delay(delayMs)
      attempt += 1
    }
  }
}

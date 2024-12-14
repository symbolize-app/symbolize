import type * as random from '@symbolize/lib-random'
import * as time from '@symbolize/lib-time'

export interface RetryConfig {
  readonly maxAttempts: number
  readonly minDelayMs: number
  readonly onError:
    | ((error: unknown, attempt: number, nextDelayMs: number) => void)
    | null
  readonly signal: AbortSignal | null
  readonly windowMs: number
}

export async function retry<Result>(
  ctx: random.Context & time.Context,
  callback: () => Promise<Result> | Result,
  config: RetryConfig,
): Promise<Result> {
  const { promise: abortPromise, resolve: abortResolve } =
    Promise.withResolvers<void>()
  config.signal?.addEventListener('abort', () => {
    abortResolve()
  })

  const startMs: number | null = ctx.time.performanceNow()
  let attempt = 0

  while (true) {
    try {
      return await callback()
    } catch (error) {
      if (attempt === config.maxAttempts - 1) {
        throw error
      }

      const nowMs = ctx.time.performanceNow()
      const delayMs =
        config.minDelayMs *
        (1 + ctx.random.number() * Math.pow(2, attempt))
      if (nowMs + delayMs >= startMs + config.windowMs) {
        throw error
      }

      config.onError?.(error, attempt, delayMs)
      await Promise.race([time.delay(ctx, delayMs), abortPromise])
      if (config.signal?.aborted) {
        throw error
      }
      attempt += 1
    }
  }
}

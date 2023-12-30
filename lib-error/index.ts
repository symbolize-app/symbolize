import type * as random from '@intertwine/lib-random'
import * as time from '@intertwine/lib-time'

export type Context = time.Context & random.Context

export type RetryConfig = {
  minDelayMs: number
  windowMs: number
  maxAttempts: number
  onError: (error: unknown, attempt: number, nextDelayMs: number) => void
}

export async function retry<Result>(
  ctx: Context,
  callback: () => Result | Promise<Result>,
  config: RetryConfig
): Promise<Result> {
  const startMs = ctx.time.performanceNow()
  let attempt = 0

  while (true) {
    try {
      return await callback()
    } catch (error) {
      if (attempt === config.maxAttempts - 1) {
        throw error
      }

      const nowMs = ctx.time.performanceNow()
      const delayMs = Math.max(
        config.minDelayMs,
        ctx.random.number() * config.minDelayMs * Math.pow(2, attempt)
      )
      if (nowMs + delayMs >= startMs + config.windowMs) {
        throw error
      }

      config.onError(error, attempt, delayMs)
      await time.delay(ctx, delayMs)
      attempt += 1
    }
  }
}

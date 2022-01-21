import type * as redisClient from '@node-redis/client'
import * as errorModule from '@tiny/core/error.ts'
import ms from 'ms'

export type Context<CacheId extends symbol> = {
  caches: { [K in CacheId]: Cache<K> }
  cacheRetryConfig: Omit<errorModule.RetryConfig, 'onError'>
}

export type Cache<CacheId extends symbol> = {
  query<Params extends unknown[], Result>(
    query: Query<CacheId, Params, Result>,
    ...params: Params
  ): Promise<Result>
}

export type Query<
  CacheId extends symbol,
  Params extends unknown[],
  Result
> = {
  cacheId: CacheId
  command: (
    client: redisClient.RedisClientType,
    ...params: Params
  ) => Promise<Result>
  errorResult: () => Result
}

export function define<
  CacheId extends symbol,
  Params extends unknown[],
  Result
>(
  cacheId: CacheId,
  query: (
    client: redisClient.RedisClientType,
    ...params: Params
  ) => Promise<Result>,
  errorResult: () => Result
): Query<CacheId, Params, Result> {
  return { cacheId, command: query, errorResult }
}

export async function retryQuery<
  CacheId extends symbol,
  Params extends unknown[],
  Result
>(
  ctx: errorModule.Context & Context<CacheId>,
  description: string,
  query: Query<CacheId, Params, Result>,
  ...params: Params
): Promise<Result> {
  try {
    return await errorModule.retry(
      ctx,
      () =>
        ctx.caches[query.cacheId].query(query, ...params),
      {
        ...ctx.cacheRetryConfig,
        onError(error, attempt, nextDelayMs) {
          console.error(
            `Retrying ${description} cache query (attempt ${attempt}, delay ${ms(
              nextDelayMs
            )})`,
            error
          )
        },
      }
    )
  } catch (error) {
    console.error(
      `Returning error result for ${description} cache query`,
      error
    )
    return query.errorResult()
  }
}

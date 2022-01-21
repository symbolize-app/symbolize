import * as redisClient from '@node-redis/client'
import type * as cacheQuery from '@tiny/cache/query.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as time from '@tiny/core/time.ts'
import chalk from 'chalk'

export const main = Symbol('cache')

export type Main = typeof main

export type MainContext = cacheQuery.Context<Main>

export const retryConfig: Omit<
  errorModule.RetryConfig,
  'onError'
> = {
  maxAttempts: 5,
  minDelayMs: time.interval({ milliseconds: 5 }),
  windowMs: time.interval({ milliseconds: 50 }),
}

export function initContext(): MainContext {
  const client = redisClient.createClient({
    url: process.env.CACHE_URL as string,
  })
  client.on('error', console.error)
  client.on('ready', () => {
    client
      .configSet('maxmemory-policy', 'allkeys-lfu')
      .catch(console.error)
    console.log(chalk.magenta(`cache client connected`))
  })
  client.on('end', () => {
    console.log(chalk.magenta(`cache client disconnected`))
  })
  client.connect().catch(console.error)
  return {
    caches: {
      [main]: {
        query<
          CacheId extends symbol,
          Params extends unknown[],
          Result
        >(
          query: cacheQuery.Query<CacheId, Params, Result>,
          ...params: Params
        ): Promise<Result> {
          return query.command(client, ...params)
        },
      },
    },
    cacheRetryConfig: retryConfig,
  }
}

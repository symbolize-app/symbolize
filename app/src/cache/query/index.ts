import * as redisClient from '@node-redis/client'
import type * as cacheQuery from '@tiny/cache/query.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as time from '@tiny/core/time.ts'
import chalk from 'chalk'

export const main = Symbol('cache')

export type Main = typeof main

export type MainContext = cacheQuery.Context<Main>

const timeout = time.interval({ milliseconds: 50 })

export const retryConfig: Omit<
  errorModule.RetryConfig,
  'onError'
> = {
  maxAttempts: 5,
  minDelayMs: time.interval({ milliseconds: 5 }),
  windowMs: time.interval({ milliseconds: 50 }),
}

export function initContext(): Exclude<
  MainContext,
  time.Context
> {
  const client = redisClient.createClient({
    url: process.env.CACHE_URL as string,
  })
  let connected = false
  client.on('error', console.error)
  client.on('ready', () => {
    connected = true
    client
      .configSet('maxmemory-policy', 'allkeys-lfu')
      .catch(console.error)
    console.log(chalk.magenta(`cache client connected`))
  })
  client.on('reconnecting', () => {
    connected = false
    console.log(chalk.magenta(`cache client reconnecting`))
  })
  client.on('end', () => {
    connected = false
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
          ctx: time.Context,
          query: cacheQuery.Query<CacheId, Params, Result>,
          ...params: Params
        ): Promise<Result> {
          if (connected) {
            return Promise.race([
              query.command(client, ...params),
              (async () => {
                await time.delay(ctx, timeout)
                throw new Error('cache command timeout')
              })(),
            ])
          } else {
            throw new Error('cache client not connected')
          }
        },
      },
    },
    cacheRetryConfig: retryConfig,
  }
}

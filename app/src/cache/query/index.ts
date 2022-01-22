import * as redisClient from '@node-redis/client'
import type * as tinyCacheQuery from '@tiny/cache/query.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinyTime from '@tiny/core/time.ts'
import chalk from 'chalk'

export const main = Symbol('cache')

export type Main = typeof main

export type MainContext = tinyCacheQuery.Context<Main>

const timeout = tinyTime.interval({ milliseconds: 50 })

export const retryConfig: Omit<
  tinyError.RetryConfig,
  'onError'
> = {
  maxAttempts: 5,
  minDelayMs: tinyTime.interval({ milliseconds: 5 }),
  windowMs: tinyTime.interval({ milliseconds: 50 }),
}

export function initContext(): Exclude<
  MainContext,
  tinyTime.Context
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
          ctx: tinyTime.Context,
          query: tinyCacheQuery.Query<
            CacheId,
            Params,
            Result
          >,
          ...params: Params
        ): Promise<Result> {
          if (connected) {
            return Promise.race([
              query.command(client, ...params),
              (async () => {
                await tinyTime.delay(ctx, timeout)
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

import * as redisClient from '@node-redis/client'
import type * as cacheQuery from '@tiny/cache/query.ts'
import chalk from 'chalk'

export type Context = {
  cache: cacheQuery.Cache
}

export function initContext(): Context {
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
    cache: {
      query<Params extends unknown[], Result>(
        query: cacheQuery.Query<Params, Result>,
        ...params: Params
      ): Promise<Result> {
        return query(client, ...params)
      },
    },
  }
}

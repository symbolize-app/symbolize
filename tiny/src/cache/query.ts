import type * as redisClient from '@node-redis/client'

export type Cache = {
  query<Params extends unknown[], Result>(
    query: Query<Params, Result>,
    ...params: Params
  ): Promise<Result>
}

export type Query<Params extends unknown[], Result> = (
  client: redisClient.RedisClientType,
  ...params: Params
) => Promise<Result>

export function define<Params extends unknown[], Result>(
  query: Query<Params, Result>
): Query<Params, Result> {
  return query
}

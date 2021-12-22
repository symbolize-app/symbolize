import * as cacheQuery from '@tiny/cache/query.ts'

export const getChunk = cacheQuery.define(
  (client, id: string, index: number) =>
    client.sendCommand<Buffer>(
      ['HGET', `file:${id}`, `${index}`],
      undefined,
      true
    )
)

export const setChunk = cacheQuery.define(
  (client, id: string, index: number, chunk: Buffer) =>
    client.hSet(`file:${id}`, `${index}`, chunk)
)

import * as cacheQuery from '@tiny/cache/query.ts'

export const getChunk = cacheQuery.define(
  async (client, id: string, index: number) => {
    return await client.sendCommand<Buffer | undefined>(
      ['HGET', `file:${id}`, `${index}`],
      undefined,
      true
    )
  }
)

export const setChunk = cacheQuery.define(
  async (
    client,
    id: string,
    index: number,
    chunk: Buffer
  ) => {
    await client.hSet(`file:${id}`, `${index}`, chunk)
  }
)

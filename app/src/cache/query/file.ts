import * as cacheQuery from '@tiny/cache/query.ts'

// The time to upload one chunk is calibrated to take about
// three fast Redis commands, to balance throughput & latency
export const chunkSizeBytes = 256 * 1024

// Pull and queue one extra chunk of data if waiting to write
export const highWaterMarkBytes = chunkSizeBytes

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

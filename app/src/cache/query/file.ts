import * as cacheQuery from '@tiny/cache/query.ts'

export const maxSizeBytes = 32 * 1024 * 1024

export const chunkSizeBytes = 64 * 1024

export const maxPipelinedChunks = 2

export const highWaterMarkBytes = chunkSizeBytes / 4

export const getChunk = cacheQuery.define(
  async (
    client,
    id: string,
    index: number
  ): Promise<Buffer | undefined> => {
    const key = `file:${id}:${index}`
    return await client.sendCommand<Buffer | undefined>(
      ['GET', key],
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
    size: number,
    parts: Uint8Array[]
  ): Promise<void> => {
    const key = `file:${id}:${index}`
    let transaction = client.multi()
    let offset = size
    for (const part of [...parts].reverse()) {
      offset -= part.length
      transaction = transaction.setRange(
        key,
        offset,
        Buffer.from(part) as unknown as string
      )
    }
    await transaction.exec()
  }
)

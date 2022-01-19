import * as appCacheQueryFile from '@fe/cache/query/file.ts'
import type * as appCacheQuery from '@fe/cache/query/index.ts'
import * as appEndpointFile from '@fe/core/endpoint/file.ts'
import type * as appDbQuery from '@fe/db/query/index.ts'
import * as route from '@tiny/api/route.ts'
import * as crypto from '@tiny/core/crypto.node.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as webStream from 'node:stream/web'

export const write = route.define(
  appEndpointFile.write,
  async (
    ctx: errorModule.Context &
      appDbQuery.WriteContext &
      appCacheQuery.Context,
    request
  ) => {
    const id = crypto.hash(
      Buffer.from(request.params.requestId, 'hex')
    )
    const response = {
      id: id.toString('hex'),
    }
    const contentLength = Number.parseInt(
      request.headers['content-length'] ?? ''
    )
    if (
      !(
        0 < contentLength &&
        contentLength < appCacheQueryFile.maxSizeBytes
      )
    ) {
      throw new route.ResponseError({
        status: 400,
        text: `invalid content length (max ${appCacheQueryFile.maxSizeBytes} bytes)`,
      })
    }
    let index = 0
    const queue: Uint8Array[] = []
    let queueLength = 0
    const pipeline: Promise<void>[] = []
    await request.stream().pipeTo(
      new webStream.WritableStream<Uint8Array>(
        {
          async write(chunk: unknown) {
            if (!(chunk instanceof Uint8Array)) {
              throw new Error('chunk wrong type')
            }
            queue.push(chunk)
            queueLength += chunk.length
            if (
              index * appCacheQueryFile.chunkSizeBytes +
                queueLength >
              contentLength
            ) {
              throw new route.ResponseError({
                status: 400,
                text: 'content too long',
              })
            }
            while (
              queueLength >=
              appCacheQueryFile.chunkSizeBytes
            ) {
              const parts = []
              let partsLength = 0
              while (true) {
                const part = queue[0]
                if (
                  partsLength + part.length >=
                  appCacheQueryFile.chunkSizeBytes
                ) {
                  const partLength =
                    appCacheQueryFile.chunkSizeBytes -
                    partsLength
                  parts.push(part.subarray(0, partLength))
                  queue[0] = part.subarray(partLength)
                  queueLength -= partLength
                  break
                } else {
                  parts.push(part)
                  partsLength += part.length
                  queue.shift()
                  queueLength -= part.length
                }
              }
              pipeline.push(
                ctx.cache.query(
                  appCacheQueryFile.setChunk,
                  response.id,
                  index,
                  appCacheQueryFile.chunkSizeBytes,
                  parts
                )
              )
              index += 1
            }
            while (
              pipeline.length >
              appCacheQueryFile.maxPipelinedChunks
            ) {
              await pipeline.shift()
            }
          },
          async close() {
            if (
              index * appCacheQueryFile.chunkSizeBytes +
                queueLength <
              contentLength
            ) {
              throw new route.ResponseError({
                status: 400,
                text: 'content too short',
              })
            }
            await ctx.cache.query(
              appCacheQueryFile.setChunk,
              response.id,
              index,
              queueLength,
              queue
            )
          },
        },
        new webStream.ByteLengthQueuingStrategy({
          highWaterMark:
            appCacheQueryFile.highWaterMarkBytes,
        })
      )
    )
    return {
      status: 200,
      json: response,
    }
  }
)

export const read = route.define(
  appEndpointFile.read,
  (
    ctx: errorModule.Context & appCacheQuery.Context,
    request
  ) => {
    // TODO Get content type & length & filename from DB
    let index = 0
    const pipeline: Promise<Buffer | undefined>[] = []
    return {
      status: 200,
      stream: new webStream.ReadableStream<Uint8Array>(
        {
          async pull(controller) {
            while (
              pipeline.length <
              appCacheQueryFile.maxPipelinedChunks
            ) {
              pipeline.push(
                ctx.cache.query(
                  appCacheQueryFile.getChunk,
                  request.params.id,
                  index
                )
              )
              index += 1
            }
            const result = await pipeline.shift()
            if (result) {
              controller.enqueue(result)
            } else {
              controller.close()
            }
          },
        },
        new webStream.ByteLengthQueuingStrategy({
          highWaterMark:
            appCacheQueryFile.highWaterMarkBytes,
        })
      ),
    }
  }
)

export const routes = [write, read]

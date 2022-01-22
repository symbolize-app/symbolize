import * as appCacheQueryFile from '@app/cache/query/file.ts'
import type * as appCacheQuery from '@app/cache/query/index.ts'
import * as appEndpointFile from '@app/core/endpoint/file.ts'
import type * as appDbQuery from '@app/db/query/index.ts'
import * as tinyRoute from '@tiny/api/route.ts'
import * as tinyCacheQuery from '@tiny/cache/query.ts'
import * as tinyCrypto from '@tiny/core/crypto.node.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as nodeWebStream from 'node:stream/web'

export const write = tinyRoute.define(
  appEndpointFile.write,
  async (
    ctx: tinyError.Context &
      appDbQuery.WriteContext &
      appCacheQuery.MainContext,
    request
  ) => {
    const id = tinyCrypto.hash(
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
      throw new tinyRoute.ResponseError({
        status: 400,
        text: `invalid content length (max ${appCacheQueryFile.maxSizeBytes} bytes)`,
      })
    }
    let index = 0
    const queue: Uint8Array[] = []
    let queueLength = 0
    const pipeline: Promise<void>[] = []
    await request.stream().pipeTo(
      new nodeWebStream.WritableStream<Uint8Array>(
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
              throw new tinyRoute.ResponseError({
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
                tinyCacheQuery.retryQuery(
                  ctx,
                  'set chunk',
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
              throw new tinyRoute.ResponseError({
                status: 400,
                text: 'content too short',
              })
            }
            await tinyCacheQuery.retryQuery(
              ctx,
              'set chunk',
              appCacheQueryFile.setChunk,
              response.id,
              index,
              queueLength,
              queue
            )
          },
        },
        new nodeWebStream.ByteLengthQueuingStrategy({
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

export const read = tinyRoute.define(
  appEndpointFile.read,
  (
    ctx: tinyError.Context & appCacheQuery.MainContext,
    request
  ) => {
    // TODO Get content type & length & filename from DB
    let index = 0
    const pipeline: Promise<Buffer | undefined>[] = []
    return {
      status: 200,
      stream: new nodeWebStream.ReadableStream<Uint8Array>(
        {
          async pull(controller) {
            while (
              pipeline.length <
              appCacheQueryFile.maxPipelinedChunks
            ) {
              pipeline.push(
                tinyCacheQuery.retryQuery(
                  ctx,
                  'get chunk',
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
        new nodeWebStream.ByteLengthQueuingStrategy({
          highWaterMark:
            appCacheQueryFile.highWaterMarkBytes,
        })
      ),
    }
  }
)

export const routes = [write, read]

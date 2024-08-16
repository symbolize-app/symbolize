import type * as random from '@intertwine/lib-random'
import * as stream from '@intertwine/lib-stream'
import * as time from '@intertwine/lib-time'

export function main(
  ctx: random.Context & stream.WorkerServerContext & time.Context,
): void {
  ctx.streamServer.serve('svc-auth-guest-read', async (serverSource) => {
    return Promise.resolve({
      async onData(data) {
        // eslint-disable-next-line no-console
        console.log('server data', data)
        await serverSource.send(ctx, 'pong')
      },
    })
  })

  void testStream(ctx)
}

async function testStream(
  ctx: random.Context & time.Context,
): Promise<void> {
  const responseSink = stream.sink<Uint8Array>((data) => {
    // eslint-disable-next-line no-console
    console.log('stream response', new TextDecoder().decode(data))
  })
  const responseUrl = new URL('/.stream', self.location.origin)
  const response = await fetch(responseUrl, {
    method: 'POST',
  })
  if (response.status !== 200) {
    throw new Error('bad response')
  }
  const responseStreamId = response.headers.get('response-stream-id')
  if (!responseStreamId) {
    throw new Error('missing response stream ID')
  }
  if (!response.body) {
    throw new Error('missing response body')
  }
  const responsePromise = response.body
    .pipeTo(responseSink.writable)
    .then(() => {
      // eslint-disable-next-line no-console
      console.log('resposne done')
    })

  const source = stream.source<Uint8Array>()
  const requestUrl = new URL('/.stream', self.location.origin)
  requestUrl.searchParams.set('response_stream_id', responseStreamId)
  const requestPromise = fetch(requestUrl, {
    body: source.readable,
    duplex: 'half',
    method: 'POST',
  })
  await source.send(ctx, new TextEncoder().encode('hello'))
  await time.delay(ctx, 10000)
  await source.send(ctx, new TextEncoder().encode('world'))
  await source.close()
  await Promise.all([requestPromise, responsePromise])
}

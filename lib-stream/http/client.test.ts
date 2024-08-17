/* eslint-disable @typescript-eslint/no-empty-function */
import type * as streamContext from '@/context.ts'
import * as streamHttpClient from '@/http/client.ts'
import type * as random from '@intertwine/lib-random'
import * as test from '@intertwine/lib-test'
//import * as time from '@intertwine/lib-time'
import type * as timeTest from '@intertwine/lib-time/test.ts'

export const url = import.meta.url

export const tests = {
  async ['run and close'](
    baseContext: random.Context & streamContext.Context & timeTest.Context,
  ): Promise<void> {
    const [onData, onDataHistory] = test.repeatMockWithHistory<
      (data: string) => void
    >(2, () => {})
    const [write, writeHistory] = test.repeatMockWithHistory<
      (chunk: string) => void
    >(2, () => {})

    const [fetch, fetchHistory] = test.mockWithHistory<
      (typeof globalThis)['fetch']
    >([
      async (...args) => {
        const request = new Request(...args)
        test.assertEquals(request.url, 'https://example.org/stream')
        return Promise.resolve(
          new Response(
            new ReadableStream({
              pull(controller) {
                controller.enqueue('c')
                controller.enqueue('d')
                controller.close()
              },
            }).pipeThrough(new TextEncoderStream()),
            {
              headers: { ['response-stream-id']: 'abcd' },
            },
          ),
        )
      },
      async (...args) => {
        const request = new Request(...args)
        test.assertEquals(
          request.url,
          'https://example.org/stream?response_stream_id=abcd',
        )
        test.assert(request.body)
        await request.body.pipeThrough(new TextDecoderStream()).pipeTo(
          new WritableStream({
            write(chunk) {
              write(chunk)
            },
          }),
        )
        return new Response()
      },
    ])

    const ctx = {
      ...baseContext,
      stream: {
        ...baseContext.stream,
        fetch,
      },
    }
    const client = streamHttpClient.httpClient(
      ctx,
      new URL('https://example.org/stream'),
      onData,
    )
    await client.source.send(ctx, 'a')
    await client.source.send(ctx, 'b')
    await client.source.close()

    test.assertEquals(fetchHistory.length, 2)
    test.assertDeepEquals(writeHistory, [['a'], ['b']])
    test.assertDeepEquals(onDataHistory, [['c'], ['d']])
  },
}

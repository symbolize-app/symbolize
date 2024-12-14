import type * as streamContext from '@/context.ts'
import * as streamSink from '@/sink.ts'
import * as streamSource from '@/source.ts'
import * as error from '@symbolize/lib-error'
import * as hex from '@symbolize/lib-hex'
import type * as random from '@symbolize/lib-random'
import type * as time from '@symbolize/lib-time'

const maxAttempts: number = 40
const minDelayMs: number = 100
const windowMs: number = 60_000

export function httpClient(
  ctx: random.Context & streamContext.Context & time.Context,
  url: Readonly<URL>,
  onData: (data: string) => Promise<void> | void,
): HttpClient {
  const client = new MutableHttpClient(url, onData)
  client.connect(ctx)
  return client
}

class MutableHttpClient {
  readonly source: streamSource.Source<string>

  private mutableResponsePromise: Promise<void> | null = null

  constructor(
    private readonly url: Readonly<URL>,
    private readonly onData: (data: string) => Promise<void> | void,
  ) {
    this.source = streamSource.source()
  }

  connect(
    ctx: random.Context & streamContext.Context & time.Context,
  ): void {
    if (!this.mutableResponsePromise) {
      this.mutableResponsePromise = this.runRetryResponseStreams(ctx)
      void (async () => {
        try {
          await this.mutableResponsePromise
        } finally {
          this.mutableResponsePromise = null
        }
      })()
    }
  }

  private async runOneRequestStream(
    ctx: streamContext.Context & time.Context,
    responseStreamId: Uint8Array,
    abortController: AbortController,
  ): Promise<void> {
    if (this.source.closed) {
      return
    }

    const requestUrl = new URL(this.url)
    requestUrl.searchParams.set(
      'response_stream_id',
      hex.uint8ArrayToHex(responseStreamId),
    )
    const beginMs = ctx.time.performanceNow()
    let error: unknown = null
    try {
      // eslint-disable-next-line no-console
      console.log(
        `HTTP request stream ${hex.uint8ArrayToHex(responseStreamId)} starting...`,
      )
      await ctx.stream.fetch(requestUrl, {
        body: this.source.readable.pipeThrough(new TextEncoderStream(), {
          preventCancel: true,
        }),
        duplex: 'half',
        method: 'POST',
        signal: abortController.signal,
      })
    } catch (error_) {
      if (
        !(error_ instanceof DOMException && error_.name === 'AbortError')
      ) {
        throw error_
      }
      error = error_
    }
    const endMs = ctx.time.performanceNow()
    const duration = endMs - beginMs

    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition -- condition may change
    if (!this.source.closed) {
      error = new Error(
        `Unexpected HTTP request stream ${hex.uint8ArrayToHex(responseStreamId)} done`,
      )
    }

    if (error !== null) {
      if (duration > windowMs / 2) {
        // eslint-disable-next-line no-console
        console.log(
          `Delayed HTTP request stream ${hex.uint8ArrayToHex(responseStreamId)} done`,
        )
      } else {
        throw error as unknown
      }
    } else {
      // eslint-disable-next-line no-console
      console.log(
        `HTTP request stream ${hex.uint8ArrayToHex(responseStreamId)} done`,
      )
    }
  }

  private async runOneResponseStream(
    ctx: random.Context & streamContext.Context & time.Context,
  ): Promise<void> {
    if (this.source.closed) {
      return
    }

    const responseSink = streamSink.sink(this.onData)
    const responseUrl = this.url
    const response = await ctx.stream.fetch(responseUrl, {
      method: 'POST',
    })
    if (response.status !== 200) {
      throw new Error('bad response')
    }
    const responseStreamIdString = response.headers.get(
      'response-stream-id',
    )
    if (!responseStreamIdString) {
      throw new Error('missing response stream ID')
    }
    const responseStreamId = hex.uint8ArrayFromHex(responseStreamIdString)
    // eslint-disable-next-line no-console
    console.log(
      `HTTP response stream ${hex.uint8ArrayToHex(responseStreamId)} started`,
    )
    if (!response.body) {
      throw new Error('missing response body')
    }
    const abortController: AbortController = new AbortController()
    const requestPromise = this.runRetryRequestStreams(
      ctx,
      responseStreamId,
      abortController,
    )
    try {
      await response.body
        .pipeThrough(new TextDecoderStream())
        .pipeTo(responseSink.writable, {
          preventAbort: true,
          preventCancel: true,
          preventClose: true,
          signal: abortController.signal,
        })
    } catch (error) {
      if (
        !(error instanceof DOMException && error.name === 'AbortError')
      ) {
        throw error
      }
    } finally {
      abortController.abort()
    }
    await requestPromise

    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition -- condition may change
    if (!this.source.closed) {
      throw new Error(
        `Unexpected HTTP response stream ${hex.uint8ArrayToHex(responseStreamId)} done`,
      )
    } else {
      // eslint-disable-next-line no-console
      console.log(
        `HTTP response stream ${hex.uint8ArrayToHex(responseStreamId)} done`,
      )
    }
  }

  private async runRetryRequestStreams(
    ctx: random.Context & streamContext.Context & time.Context,
    responseStreamId: Uint8Array,
    abortController: AbortController,
  ): Promise<void> {
    if (!abortController.signal.aborted) {
      try {
        await error.retry(
          ctx,
          async () =>
            this.runOneRequestStream(
              ctx,
              responseStreamId,
              abortController,
            ),
          {
            maxAttempts,
            minDelayMs,
            signal: abortController.signal,
            windowMs,

            // eslint-disable-next-line @typescript-eslint/no-loop-func
            onError(error, attempt, nextDelayMs) {
              // eslint-disable-next-line no-console
              console.error(
                `Error connnecting response stream (attempt ${attempt}, next delay ${nextDelayMs} ms)`,
                error,
              )
            },
          },
        )
      } finally {
        abortController.abort()
        // eslint-disable-next-line no-console
        console.log('HTTP request stream retry done')
      }
    }
  }

  private async runRetryResponseStreams(
    ctx: random.Context & streamContext.Context & time.Context,
  ): Promise<void> {
    // TODO loop
    try {
      await error.retry(ctx, async () => this.runOneResponseStream(ctx), {
        maxAttempts,
        minDelayMs,
        signal: null,
        windowMs,

        onError(error, attempt, nextDelayMs) {
          // eslint-disable-next-line no-console
          console.error(
            `Error connnecting response stream (attempt ${attempt}, next delay ${nextDelayMs} ms)`,
            error,
          )
        },
      })
    } finally {
      // eslint-disable-next-line no-console
      console.log('HTTP response stream retry done')
    }
  }
}

export type HttpClient = Readonly<MutableHttpClient>

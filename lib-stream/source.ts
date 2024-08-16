import * as time from '@intertwine/lib-time'

const highWaterMark = 16
const timeoutMs = 1_000

export function source<T>(): Source<T> {
  return new Source()
}

class Source<T> {
  readonly readable: ReadableStream<T>
  private readonly writer: WritableStreamDefaultWriter<T>

  constructor() {
    const transform = new TransformStream<T, T>(undefined, {
      highWaterMark,
    })
    this.readable = transform.readable
    this.writer = transform.writable.getWriter()
  }

  async close(): Promise<void> {
    return this.writer.close()
  }

  async send(ctx: time.Context, data: T): Promise<void> {
    const readyBeforeTimeout = await Promise.race([
      this.writer.write(data).then(() => true),
      time.delay(ctx, timeoutMs).then(() => false),
    ])
    if (!readyBeforeTimeout) {
      throw new Error('Send timeout')
    }
  }
}

export type { Source }

import * as time from '@intertwine/lib-time'

const highWaterMark = 16
const timeoutMs = 1_000

export class Source<T> {
  private readonly writer: WritableStreamDefaultWriter<T>
  readonly readable: ReadableStream<T>

  constructor() {
    const transform = new TransformStream<T, T>(undefined, {
      highWaterMark,
    })
    this.writer = transform.writable.getWriter()
    this.readable = transform.readable
  }

  async send(ctx: time.Context, data: T): Promise<void> {
    const ready = await Promise.race([
      this.writer.ready.then(() => true),
      time.delay(ctx, timeoutMs).then(() => false),
    ])
    if (!ready) {
      throw new Error('Not ready, cannot write')
    }
    await this.writer.write(data)
  }
}

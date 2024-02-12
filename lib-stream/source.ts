import * as time from '@intertwine/lib-time'

const highWaterMark = 16
const timeoutMs = 1_000

export class Source<T> {
  private constructor(
    private readonly writer: WritableStreamDefaultWriter<T>,
    readonly readable: ReadableStream<T>,
  ) {}

  static build<T>(): Source<T> {
    const transform = new TransformStream<T, T>(undefined, {
      highWaterMark,
    })
    return new Source<T>(
      transform.writable.getWriter(),
      transform.readable,
    )
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

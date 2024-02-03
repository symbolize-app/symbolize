const highWaterMark = 16
const limit = 8

export class Sink<T> {
  readonly writable: WritableStream<T>
  private readonly active: Set<Promise<void>> = new Set<Promise<void>>()

  constructor(onData: (data: T) => Promise<void>) {
    const active = this.active
    this.writable = new WritableStream(
      {
        async write(data) {
          const current = onData(data).finally(() =>
            active.delete(current)
          )
          active.add(current)
          while (active.size >= limit) {
            await Promise.race([...active.values()])
          }
        },
      },
      { highWaterMark }
    )
  }
}

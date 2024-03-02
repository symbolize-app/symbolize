const highWaterMark = 16
const limit = 8

export class Sink<T> {
  private readonly mutableActive: Set<Promise<void>> = new Set<
    Promise<void>
  >()

  private constructor(
    private readonly onData: (data: T) => Promise<void> | void,
    readonly writable: WritableStream<T>,
  ) {}

  static build<T>(onData: (data: T) => Promise<void> | void): Sink<T> {
    const sink: Sink<T> = new Sink<T>(
      onData,
      new WritableStream(
        {
          async write(data) {
            return sink.write(data)
          },
        },
        { highWaterMark },
      ),
    )
    return sink
  }

  async write(data: T): Promise<void> {
    const current = Promise.resolve(this.onData(data)).finally(() =>
      this.mutableActive.delete(current),
    )
    this.mutableActive.add(current)
    while (this.mutableActive.size >= limit) {
      await Promise.race([...this.mutableActive.values()])
    }
  }
}

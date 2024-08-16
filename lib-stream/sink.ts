const highWaterMark = 16
const limit = 8

export function sink<T>(
  onData: (data: T) => Promise<void> | void,
): Sink<T> {
  return new Sink(onData)
}

class Sink<T> {
  readonly writable: WritableStream<T>

  constructor(onData: (data: T) => Promise<void> | void) {
    this.writable = new WritableStream(new UnderlyingSinkImpl(onData), {
      highWaterMark,
    })
  }
}

export type { Sink }

class UnderlyingSinkImpl<T> implements UnderlyingSink<T> {
  private readonly mutableActive: Set<Promise<void>> = new Set<
    Promise<void>
  >()

  constructor(
    private readonly onData: (data: T) => Promise<void> | void,
  ) {}

  async write(chunk: T): Promise<void> {
    const current = Promise.resolve(this.onData(chunk)).finally(() =>
      this.mutableActive.delete(current),
    )
    this.mutableActive.add(current)
    while (this.mutableActive.size >= limit) {
      await Promise.race([...this.mutableActive.values()])
    }
  }
}

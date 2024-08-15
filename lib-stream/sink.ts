const highWaterMark = 16
const limit = 8

class Sink<T> {
  constructor(readonly writable: WritableStream<T>) {}
}

export type { Sink }

export function sink<T>(
  onData: (data: T) => Promise<void> | void,
): Sink<T> {
  const mutableActive: Set<Promise<void>> = new Set<Promise<void>>()

  const sink: Sink<T> = new Sink<T>(
    new WritableStream(
      {
        async write(data) {
          const current = Promise.resolve(onData(data)).finally(() =>
            mutableActive.delete(current),
          )
          mutableActive.add(current)
          while (mutableActive.size >= limit) {
            await Promise.race([...mutableActive.values()])
          }
        },
      },
      { highWaterMark },
    ),
  )
  return sink
}

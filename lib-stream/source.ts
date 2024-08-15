import * as concurrency from '@intertwine/lib-concurrency'
import * as time from '@intertwine/lib-time'

const highWaterMark = 16
const timeoutMs = 1_000

class Source<T> {
  constructor(
    private readonly onClose: () => void,
    private readonly onSend: (ctx: time.Context, data: T) => Promise<void>,
    readonly readable: ReadableStream<T>,
  ) {}

  close(): void {
    this.onClose()
  }

  async send(ctx: time.Context, data: T): Promise<void> {
    await this.onSend(ctx, data)
  }
}

export type { Source }

export function source<T>(): Source<T> {
  let controller: ReadableStreamDefaultController | null = null
  const ready = concurrency.eventSemaphore()
  const done = concurrency.eventSemaphore()
  return new Source<T>(
    () => {
      if (!controller) {
        throw new Error('Closed before started')
      }
      controller.close()
      done.set()
    },
    async (ctx, data) => {
      const readyBeforeTimeout = await Promise.race([
        ready.wait().then(() => true),
        time.delay(ctx, timeoutMs).then(() => false),
      ])
      if (!readyBeforeTimeout) {
        throw new Error('Ready before started')
      }
      if (!controller) {
        throw new Error('Ready but missing controller')
      }
      controller.enqueue(data)
      if (controller.desiredSize === null) {
        done.set()
        throw new Error('Stream in error state')
      } else if (controller.desiredSize <= 0) {
        ready.clear()
        done.set()
      }
    },
    new ReadableStream(
      {
        async pull() {
          done.clear()
          ready.set()
          await done.wait()
        },
        start(controller_) {
          controller = controller_
        },
      },
      { highWaterMark },
    ),
  )
}

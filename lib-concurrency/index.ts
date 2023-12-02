export class EventSemaphore {
  ready = false
  queue: (() => void)[]

  constructor() {
    this.queue = []
  }

  set(): void {
    this.ready = true
    while (true) {
      const item = this.queue.shift()
      if (item) {
        item()
      } else {
        break
      }
    }
  }

  clear(): void {
    this.ready = false
  }

  wait(): Promise<void> {
    return new Promise((resolve) => {
      if (this.ready) {
        resolve()
      } else {
        this.queue.push(resolve)
      }
    })
  }
}

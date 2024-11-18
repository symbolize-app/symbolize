class EventSemaphore {
  private mutableReady: Promise<void>
  private mutableResolve: () => void
  private mutableResolved: boolean = true

  constructor() {
    ;({ promise: this.mutableReady, resolve: this.mutableResolve } =
      Promise.withResolvers())
    this.mutableResolved = false
  }

  get ready(): Promise<void> {
    return this.waitReady()
  }

  clear(): void {
    if (this.mutableResolved) {
      ;({ promise: this.mutableReady, resolve: this.mutableResolve } =
        Promise.withResolvers())
      this.mutableResolved = false
    }
  }

  set(): void {
    if (!this.mutableResolved) {
      this.mutableResolve()
      this.mutableResolved = true
    }
  }

  private async waitReady(): Promise<void> {
    while (true) {
      await this.mutableReady
      if (this.mutableResolved) {
        return
      }
    }
  }
}

export type { EventSemaphore }

export function eventSemaphore(): Readonly<EventSemaphore> {
  const eventSemaphore = new EventSemaphore()
  eventSemaphore.clear()
  return eventSemaphore
}

class EventSemaphore {
  private mutableReady!: Promise<void>
  private mutableResolve!: () => void
  private mutableResolved: boolean = true

  clear(): void {
    if (this.mutableResolved) {
      this.mutableReady = new Promise((resolve) => {
        this.mutableResolve = resolve
      })
      this.mutableResolved = false
    }
  }

  set(): void {
    if (!this.mutableResolved) {
      this.mutableResolve()
      this.mutableResolved = true
    }
  }

  async wait(): Promise<void> {
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

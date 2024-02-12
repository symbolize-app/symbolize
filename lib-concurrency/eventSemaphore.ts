export class EventSemaphore {
  private mutableReady!: Promise<void>
  private mutableResolve!: () => void
  private mutableResolved: boolean = true

  private constructor() {
    // Private
  }

  static build(): Readonly<EventSemaphore> {
    const eventSemaphore = new EventSemaphore()
    eventSemaphore.clear()
    return eventSemaphore
  }

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
    await this.mutableReady
  }
}

export class EventSemaphore {
  private ready!: Promise<void>
  private resolve!: () => void
  private resolved: boolean = true

  constructor() {
    this.clear()
  }

  set(): void {
    if (!this.resolved) {
      this.resolve()
      this.resolved = true
    }
  }

  clear(): void {
    if (this.resolved) {
      this.ready = new Promise((resolve) => {
        this.resolve = resolve
      })
      this.resolved = false
    }
  }

  async wait(): Promise<void> {
    await this.ready
  }
}

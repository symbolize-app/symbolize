export class Scheduler {
  private mutableQueueTail: Promise<void> = Promise.resolve()

  async run(callback: () => Promise<void>): Promise<void> {
    const currentQueueTail = this.mutableQueueTail
    const dispatch = async (): Promise<void> => {
      await currentQueueTail
      await callback()
    }
    return (this.mutableQueueTail = dispatch())
  }

  async wait(): Promise<void> {
    return this.mutableQueueTail
  }
}

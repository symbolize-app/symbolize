export class Scheduler {
  private readonly mutableQueue: (() => Promise<void>)[] = []

  async run(callback: () => Promise<void>): Promise<void> {
    return new Promise((resolve) => {
      const dispatch = async (): Promise<void> => {
        await callback()
        resolve()
        this.mutableQueue.shift()
        await this.mutableQueue[0]?.()
      }
      this.mutableQueue.push(dispatch)
      if (this.mutableQueue.length === 1) {
        void dispatch()
      }
    })
  }
}

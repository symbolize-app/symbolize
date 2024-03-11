import type * as conveyContext from '@/context.ts'
import * as conveyScheduler from '@/scheduler.ts'
import * as happyDom from 'happy-dom'

export class ConveyImpl implements conveyContext.Convey {
  readonly scheduler: conveyScheduler.Scheduler =
    new conveyScheduler.Scheduler()

  private mutableDocument: happyDom.Document | null = null
  private mutableWindow: happyDom.Window | null = null

  get document(): conveyContext.Convey['document'] {
    if (!this.mutableDocument) {
      this.mutableWindow = new happyDom.Window({
        url: 'https://localhost:8080',
      })
      this.mutableDocument = this.mutableWindow.document
    }
    return this
      .mutableDocument as unknown as conveyContext.Convey['document']
  }

  dispose(): void {
    if (this.mutableWindow) {
      this.mutableWindow.close()
    }
  }
}

import * as convey from '@/index.ts'
import * as happyDom from 'happy-dom'

export class ConveyImpl implements convey.Convey {
  readonly scheduler: convey.ConveyScheduler = new convey.ConveyScheduler()

  private mutableDocument: happyDom.Document | null = null
  private mutableWindow: happyDom.Window | null = null

  get document(): convey.Convey['document'] {
    if (!this.mutableDocument) {
      this.mutableWindow = new happyDom.Window({
        url: 'https://localhost:8080',
      })
      this.mutableDocument = this.mutableWindow.document
    }
    return this.mutableDocument as unknown as convey.Convey['document']
  }

  dispose(): void {
    if (this.mutableWindow) {
      this.mutableWindow.close()
    }
  }
}

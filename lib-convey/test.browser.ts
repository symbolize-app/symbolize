import * as convey from '@/index.ts'

export class ConveyImpl implements convey.Convey {
  readonly scheduler: convey.ConveyScheduler = new convey.ConveyScheduler()

  private mutableDocument: Document | null = null
  private mutableIframe: HTMLIFrameElement | null = null

  get document(): convey.Convey['document'] {
    if (!this.mutableDocument) {
      const mutableIframe = window.document.createElement('iframe')
      mutableIframe.style.display = 'none'
      window.document.body.append(mutableIframe)
      const iframeDocument = mutableIframe.contentDocument
      if (!iframeDocument) {
        throw new Error('No iframe document')
      }
      this.mutableIframe = mutableIframe
      this.mutableDocument = iframeDocument
    }
    return this.mutableDocument
  }

  dispose(): void {
    if (this.mutableIframe) {
      this.mutableIframe.remove()
    }
  }
}

import type * as conveyContext from '@/context.ts'
import * as conveyScheduler from '@/scheduler.ts'

export class ConveyImpl implements conveyContext.Convey {
  readonly classNames = new Set<string>()
  readonly document: Readonly<Document> = window.document
  readonly mutableScheduler: conveyScheduler.Scheduler =
    new conveyScheduler.Scheduler()
  readonly styleLayer: Readonly<CSSLayerBlockRule>

  constructor() {
    const style = this.document.createElement('style')
    this.document.head.append(style)
    const styleSheet = style.sheet
    if (!styleSheet) {
      throw new Error('No style sheet')
    }
    this.styleLayer = styleSheet.cssRules.item(
      styleSheet.insertRule('@layer base {}'),
    ) as CSSLayerBlockRule
  }
}

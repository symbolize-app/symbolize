import type * as hypertextContext from '@/context.ts'
import * as hypertextScheduler from '@/scheduler.ts'

class HypertextImpl implements hypertextContext.Hypertext {
  readonly classNames = new Set<string>()
  readonly document: Readonly<Document> = window.document
  readonly scheduler: hypertextScheduler.Scheduler =
    hypertextScheduler.scheduler()
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

export function hypertext(): hypertextContext.Hypertext {
  return new HypertextImpl()
}

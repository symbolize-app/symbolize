import type * as markupContext from '@/context.ts'
import * as markupScheduler from '@/scheduler.ts'

class MarkupImpl implements markupContext.Markup {
  readonly classNames = new Set<string>()
  readonly document: Readonly<Document> = window.document
  readonly scheduler: markupScheduler.Scheduler =
    markupScheduler.scheduler()
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

export function markup(): markupContext.Markup {
  return new MarkupImpl()
}

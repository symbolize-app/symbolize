import type * as markupContext from '@/context.ts'
import * as markupScheduler from '@/scheduler.ts'
import * as happyDom from 'happy-dom'

export interface Markup extends markupContext.Markup {
  dispose(): void
}

class MarkupImpl implements Markup {
  readonly classNames = new Set<string>()
  readonly document: markupContext.RestrictedDocument
  readonly scheduler: markupScheduler.Scheduler =
    markupScheduler.scheduler()

  readonly styleLayer: markupContext.RestrictedCssLayerBlockRule
  private readonly window: Readonly<happyDom.Window> = new happyDom.Window(
    {
      url: 'https://localhost:8080',
    },
  )

  constructor() {
    this.document = this.window
      .document as unknown as markupContext.RestrictedDocument
    const mutableStyleRules: markupContext.RestrictedCssRule[] = []
    this.styleLayer = {
      cssRules: mutableStyleRules,
      insertRule(rule, index?) {
        const styleRule: markupContext.RestrictedCssRule = {
          cssText: rule,
        }
        mutableStyleRules.splice(index ?? 0, 0, styleRule)
        return index ?? 0
      },
    }
  }

  dispose(): void {
    this.window.close()
  }
}

export function markup(): Markup {
  return new MarkupImpl()
}

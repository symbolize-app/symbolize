import type * as hypertextContext from '@/context.ts'
import * as hypertextScheduler from '@/scheduler.ts'
import * as happyDom from 'happy-dom'

export interface Hypertext extends hypertextContext.Hypertext {
  dispose(): void
}

class HypertextImpl implements Hypertext {
  readonly classNames = new Set<string>()
  readonly document: hypertextContext.RestrictedDocument
  readonly scheduler: hypertextScheduler.Scheduler =
    hypertextScheduler.scheduler()

  readonly styleLayer: hypertextContext.RestrictedCssLayerBlockRule
  private readonly window: Readonly<happyDom.Window> = new happyDom.Window(
    {
      url: 'https://localhost:8080',
    },
  )

  constructor() {
    this.document = this.window
      .document as unknown as hypertextContext.RestrictedDocument
    const mutableStyleRules: hypertextContext.RestrictedCssRule[] = []
    this.styleLayer = {
      cssRules: mutableStyleRules,
      insertRule(rule, index?) {
        const styleRule: hypertextContext.RestrictedCssRule = {
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

export function hypertext(): Hypertext {
  return new HypertextImpl()
}

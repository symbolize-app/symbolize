import type * as conveyContext from '@/context.ts'
import * as conveyScheduler from '@/scheduler.ts'
import * as happyDom from 'happy-dom'

export class ConveyImpl implements conveyContext.Convey {
  readonly classNames = new Set<string>()
  readonly document: conveyContext.RestrictedDocument
  readonly mutableScheduler: conveyScheduler.Scheduler =
    new conveyScheduler.Scheduler()

  readonly styleLayer: conveyContext.RestrictedCssLayerBlockRule
  private readonly window: Readonly<happyDom.Window> = new happyDom.Window(
    {
      url: 'https://localhost:8080',
    },
  )

  constructor() {
    this.document = this.window
      .document as unknown as conveyContext.RestrictedDocument
    const mutableStyleRules: conveyContext.RestrictedCssRule[] = []
    this.styleLayer = {
      cssRules: mutableStyleRules,
      insertRule(rule, index?) {
        const styleRule: conveyContext.RestrictedCssRule = {
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

import type * as conveyContext from '@/context.ts'
import * as conveyScheduler from '@/scheduler.ts'
import * as happyDom from 'happy-dom'

export interface Convey extends conveyContext.Convey {
  dispose(): void
}

class ConveyImpl implements Convey {
  readonly classNames = new Set<string>()
  readonly document: conveyContext.RestrictedDocument
  readonly scheduler: conveyScheduler.Scheduler =
    conveyScheduler.scheduler()

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

export function convey(): Convey {
  return new ConveyImpl()
}

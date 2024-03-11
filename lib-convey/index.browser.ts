import type * as conveyContext from '@/context.ts'
import * as conveyScheduler from '@/scheduler.ts'

export class ConveyImpl implements conveyContext.Convey {
  readonly document: conveyContext.Convey['document'] = window.document
  readonly scheduler: conveyScheduler.Scheduler =
    new conveyScheduler.Scheduler()
}

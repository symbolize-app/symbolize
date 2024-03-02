import type * as convey from '@/index.ts'

export class ConveyImpl implements convey.Convey {
  readonly document: convey.Convey['document'] = window.document
}

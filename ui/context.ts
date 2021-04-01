import type * as submit from '@tiny/api/submit.ts'
import type * as widget from '@tiny/ui/widget.ts'
import type * as errorModule from '@tiny/util/error.ts'

export type Context = widget.WidgetContext &
  errorModule.RetryContext &
  submit.SubmitContext

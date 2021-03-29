import type * as widget from '@tiny/ui/widget.ts'
import type * as errorModule from '@tiny/util/error.ts'

export type Context = widget.WidgetContext &
  errorModule.RetryContext &
  FetchContext

export type FetchContext = { fetch: typeof window.fetch }

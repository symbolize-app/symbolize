import * as button from '@fe/ui/button.ts'
import * as test from '@tiny/test/index.ts'
import * as widgetTest from '@tiny/ui/widget.test.ts'
import type * as widget from '@tiny/ui/widget.ts'

export const url = import.meta.url

export const tests = {
  ['button text'](ctx: widget.WidgetContext): void {
    const buttonElement = widgetTest.collectOne<HTMLElement>(
      button.custom(ctx, {})
    )
    test.assertEquals(buttonElement.textContent, 'OK')
  },
}

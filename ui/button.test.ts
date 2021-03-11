import * as button from '@fe/ui/button.ts'
import * as widgetTest from '@tiny/ui/widget.test.ts'
import type * as widget from '@tiny/ui/widget.ts'
import chai from 'chai'

export const url = import.meta.url

export const tests = {
  ['button text'](ctx: widget.WidgetContext): void {
    const buttonElement = widgetTest.collectOne(
      button.custom(ctx, {})
    )
    chai.expect(buttonElement).to.have.text('OK')
  },
}

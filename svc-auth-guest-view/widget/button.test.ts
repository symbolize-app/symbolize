import * as svcWidgetButton from '@/widget/button.ts'
import type {} from '@intertwine/lib-style'
import * as test from '@intertwine/lib-test'
import type * as widget from '@intertwine/lib-widget'
import * as widgetTest from '@intertwine/lib-widget/index.test.ts'

export const url = import.meta.url

export const tests = {
  ['button text']: widgetTest.withTempDocument((ctx: widget.Context) => {
    ctx.widget.document.body.content = [svcWidgetButton.custom(ctx, {})]
    const button =
      ctx.widget.document.body.querySelector<HTMLButtonElement>(
        ':scope > button'
      )
    test.assertEquals(button?.textContent, 'OK')
  }),
}

import * as test from '@intertwine/lib-test'
import type * as widget from '@intertwine/lib-widget'
import * as widgetTest from '@intertwine/lib-widget/index.test.ts'

import * as svcWidgetButton from '@/widget/button.ts'

export const url = import.meta.url

export const tests = {
  ['button text']: widgetTest.withTempDocument(
    (ctx: widget.Context) => {
      ctx.document.body.content = [
        svcWidgetButton.custom(ctx, {}),
      ]
      const button =
        ctx.document.body.querySelector<HTMLButtonElement>(
          ':scope > button'
        )
      test.assertEquals(button?.textContent, 'OK')
    }
  ),
}

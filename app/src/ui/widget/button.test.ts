import * as appWidgetButton from '@fe/ui/widget/button.ts'
import * as test from '@tiny/test/index.ts'
import * as widgetTest from '@tiny/ui/widget.test.ts'
import type * as widget from '@tiny/ui/widget.ts'

export const url = import.meta.url

export const tests = {
  ['button text']: widgetTest.withTempDocument(
    (ctx: widget.Context) => {
      ctx.document.body.content = [
        appWidgetButton.custom(ctx, {}),
      ]
      const button = ctx.document.body.querySelector<HTMLButtonElement>(
        ':scope > button'
      )
      test.assertEquals(button?.textContent, 'OK')
    }
  ),
}

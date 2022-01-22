import * as appWidgetButton from '@app/ui/widget/button.ts'
import * as tinyTest from '@tiny/test/index.ts'
import * as tinyWidgetTest from '@tiny/ui/widget.test.ts'
import type * as tinyWidget from '@tiny/ui/widget.ts'

export const url = import.meta.url

export const tests = {
  ['button text']: tinyWidgetTest.withTempDocument(
    (ctx: tinyWidget.Context) => {
      ctx.document.body.content = [
        appWidgetButton.custom(ctx, {}),
      ]
      const button =
        ctx.document.body.querySelector<HTMLButtonElement>(
          ':scope > button'
        )
      tinyTest.assertEquals(button?.textContent, 'OK')
    }
  ),
}

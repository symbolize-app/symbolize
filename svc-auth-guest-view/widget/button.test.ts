import * as tinyTest from '@intertwine/lib-test/index.ts'
import * as tinyWidgetTest from '@intertwine/lib-widget/widget.test.ts'
import type * as tinyWidget from '@intertwine/lib-widget/widget.ts'

import * as appWidgetButton from '@/widget/button.ts'

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

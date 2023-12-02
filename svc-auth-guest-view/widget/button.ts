import * as tinyWidget from '@intertwine/lib-widget/widget.ts'

const button = tinyWidget.html.button

export const custom = tinyWidget.define(
  (
    ctx: tinyWidget.Context
  ): {
    body: tinyWidget.Widget
    listen: tinyWidget.HtmlListeners
  } => {
    const body = button(ctx, {
      content: ['OK'],
    })
    return {
      body,
      set listen(value: tinyWidget.HtmlListeners) {
        body.listen = value
      },
    }
  }
)

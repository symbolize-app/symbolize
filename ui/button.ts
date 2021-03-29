import * as widget from '@tiny/ui/widget.ts'

const button = widget.html.button

export const custom = widget.define<{
  body: widget.Widget
  listen: widget.HtmlListeners
}>((ctx) => {
  const body = button(ctx, {
    content: ['OK'],
  })
  return {
    body,
    set listen(value: widget.HtmlListeners) {
      body.listen = value
    },
  }
})

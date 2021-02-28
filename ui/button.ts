import * as widget from '@tiny/ui/widget.ts'

const button = widget.html.button

export const custom = widget.define<{
  readonly body: widget.Widget
  listen: widget.HtmlListeners
}>(() => {
  const body = button({
    content: ['OK'],
  })
  return {
    body,
    set listen(value: widget.HtmlListeners) {
      body.listen = value
    },
  }
})

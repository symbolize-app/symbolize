import * as uiTest from '@fe/ui/index.test.browser.ts'
import type * as time from '@tiny/core/time.ts'
import type * as widget from '@tiny/ui/widget.ts'

export function main(
  ctx: widget.Context & time.Context
): void {
  uiTest.run(ctx).catch(console.error)

  const socket = new WebSocket(
    `${window.location.origin.replace(
      /^http/,
      'ws'
    )}/api/dev`
  )
  socket.addEventListener('open', () => {
    console.log(
      '%cdev socket connected',
      'font-style: italic; color: blue'
    )
  })
  socket.addEventListener('close', () => {
    console.log(
      '%cdev socket closed',
      'font-style: italic; color: blue'
    )
  })
  socket.addEventListener('message', (event) => {
    if (event.data === 'reload') {
      window.location.reload()
    }
  })
}

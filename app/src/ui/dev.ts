import * as appUiTest from '@app/ui/index.test.browser.ts'
import type * as tinyTime from '@tiny/core/time.ts'
import type * as tinyWidget from '@tiny/ui/widget.ts'

export function main(
  ctx: tinyWidget.Context & tinyTime.Context
): void {
  appUiTest.run(ctx).catch(console.error)

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

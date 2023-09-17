import type * as tinyTime from '@intertwine/lib-time/time.ts'
import type * as tinyWidget from '@intertwine/lib-widget/widget.ts'

import * as appUiTest from '@/index.test.browser.ts'

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

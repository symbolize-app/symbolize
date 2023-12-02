import * as test from '@intertwine/lib-test'
import type * as time from '@intertwine/lib-time'
import type * as widget from '@intertwine/lib-widget'

export function main(
  ctx: widget.Context & time.Context
): void {
  test
    .runAll(ctx, [import('@intertwine/dev-pnpm-test')])
    .catch(console.error)

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

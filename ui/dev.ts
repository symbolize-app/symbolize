import * as uiTest from '@fe/ui/index.test.ts'

export function main(): void {
  uiTest.run().catch(console.error)

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

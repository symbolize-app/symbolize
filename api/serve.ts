import * as message from '@fe/core/message.ts'
import * as http from 'http'

export function handleRequest(
  req: http.IncomingMessage,
  res: http.ServerResponse
): void {
  console.log('MESSAGE', message.hi)
  console.log('GOT', req.url)
  res.write(message.hi)
  res.end()
}

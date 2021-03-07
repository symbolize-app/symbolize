import * as message from '@fe/core/message.ts'
import * as http from 'http'
import * as urlModule from 'url'

function main() {
  const server = http.createServer(handleRequest)
  server.listen(8001)
}

export function handleRequest(
  req: http.IncomingMessage,
  res: http.ServerResponse
): void {
  console.log('MESSAGE', message.hi)
  console.log('GOT', req.url)
  res.write(message.hi)
  res.end()
}

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void main()
}

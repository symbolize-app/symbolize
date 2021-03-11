import * as message from '@fe/core/message.ts'
import * as http from 'http'
import * as urlModule from 'url'

async function main() {
  const server = http.createServer(handleRequest)
  server.listen(process.env.PORT)
  if (process.env.NODE_ENV === 'development') {
    const dev = await import('@fe/api/dev.ts')
    dev.main()
  }
}

export function handleRequest(
  _req: http.IncomingMessage,
  res: http.ServerResponse
): void {
  res.write(message.hi)
  res.end()
}

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void main()
}

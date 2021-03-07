import * as route from '@tiny/api/route.ts'
import * as fs from 'fs'
import * as http from 'http'
import urlModule from 'url'

const index = route.define(['GET'], /^\/$/, () => {
  return {
    status: 200,
    headers: {
      'content-type': 'text/html',
    },
    body: fs.createReadStream('public/index.html'),
  }
})

const js = route.define(
  ['GET'],
  /^\/js\/(?<path>.+\.mjs)/,
  (request) => {
    return {
      status: 200,
      headers: {
        'content-type': 'application/javascript',
      },
      body: `console.log("HI", ${JSON.stringify(
        request.match.path
      )}, ${JSON.stringify(request.url)})`,
    }
  }
)

const notFound = route.define(undefined, /.*/, () => {
  return {
    status: 404,
    headers: {},
    body: '',
  }
})

function main() {
  const server = http.createServer(
    route.handle([index, js, notFound])
  )
  server.listen(process.env.DEV_PORT)
}

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void main()
}

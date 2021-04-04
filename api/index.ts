import * as apiQuery from '@fe/api/query.ts'
import * as db from '@fe/db/index.ts'
import * as button from '@fe/ui/button.ts'
import * as route from '@tiny/api/route.ts'
import * as widget from '@tiny/ui/widget.ts'
import type * as errorModule from '@tiny/util/error.ts'
import * as random from '@tiny/util/random.ts'
import * as timeNode from '@tiny/util/time.node.ts'
import chalk from 'chalk'
import * as fs from 'fs'
import * as http from 'http'
import jsdom from 'jsdom'
import type * as net from 'net'
import urlModule from 'url'

const index = route.define(['GET'], /^\/$/, () => {
  return {
    status: 200,
    headers: {
      'content-type': 'text/html',
    },
    body: fs.createReadStream('build/browser/index.html'),
  }
})

const ssr = route.define(['GET'], /^\/ssr$/, () => {
  const dom = new jsdom.JSDOM('<!DOCTYPE html>')
  const window = dom.window
  const document = window.document
  const ctx = {
    ...widget.initContext(document),
  }
  const body = widget.toHtmlWidget(ctx, document.body)
  body.content = [button.custom(ctx, {})]
  return {
    status: 200,
    headers: {
      'content-type': 'text/html',
    },
    body: dom.serialize(),
  }
})

const notFound = route.define(undefined, /.*/, () => {
  return {
    status: 404,
    headers: {},
    body: '',
  }
})

const js = route.define(
  ['GET'],
  /^\/js\/(?<path>.+\.mjs)$/,
  (_ctx, request) => {
    return {
      status: 200,
      headers: {
        'content-type': 'application/javascript',
      },
      body: fs.createReadStream(
        `build/browser/js/${request.match.path}`
      ),
    }
  }
)

async function main(): Promise<void> {
  const ctx: errorModule.Context &
    db.ReadContext &
    db.WriteContext = {
    ...random.initContext(),
    ...timeNode.initContext(),
    ...db.initContext(),
  }
  const httpServer = http.createServer(
    route.handle(ctx, [
      index,
      ssr,
      js,
      ...apiQuery.routes,
      notFound,
    ])
  )
  httpServer.on('error', console.error)
  httpServer.listen(process.env.PORT)

  if (process.env.NODE_ENV === 'development') {
    const dev = await import('@fe/api/dev.ts')
    dev.main(ctx)
  } else {
    console.log(
      chalk.bold(
        `Ready at http://localhost:${
          (httpServer.address() as net.AddressInfo).port
        }/`
      )
    )
  }
}

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void main()
}

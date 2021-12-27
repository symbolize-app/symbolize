import * as appFts from '@fe/api/fts.ts'
import * as appRouteMember from '@fe/api/route/member.ts'
import * as appRouteMessage from '@fe/api/route/message.ts'
import * as appRouteSearch from '@fe/api/route/search.ts'
import * as appRouteTopic from '@fe/api/route/topic.ts'
import * as appCacheQuery from '@fe/cache/query/index.ts'
import * as appDbQuery from '@fe/db/query/index.ts'
import * as appWidgetButton from '@fe/ui/widget/button.ts'
import * as route from '@tiny/api/route.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as random from '@tiny/core/random.ts'
import * as submitNode from '@tiny/core/submit.node.ts'
import type * as submit from '@tiny/core/submit.ts'
import * as timeNode from '@tiny/core/time.node.ts'
import * as widget from '@tiny/ui/widget.ts'
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
    stream: fs.createReadStream('build/browser/index.html'),
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
  body.content = [appWidgetButton.custom(ctx, {})]
  return {
    status: 200,
    headers: {
      'content-type': 'text/html',
    },
    text: dom.serialize(),
  }
})

const notFound = route.define(undefined, /.*/, () => {
  return {
    status: 404,
    headers: {},
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
      stream: fs.createReadStream(
        `build/browser/js/${request.match.path}`
      ),
    }
  }
)

function main(): void {
  const ctx: errorModule.Context &
    appCacheQuery.Context &
    appDbQuery.ReadContext &
    appDbQuery.WriteContext &
    submit.Context &
    appFts.Context = {
    ...random.initContext(),
    ...timeNode.initContext(),
    ...appCacheQuery.initContext(),
    ...appDbQuery.initContext(),
    ...submitNode.initContext(),
    ...appFts.initContext(),
  }
  const httpServer = http.createServer(
    route.handle(ctx, [
      index,
      ssr,
      js,
      ...appRouteMember.routes,
      ...appRouteMessage.routes,
      ...appRouteSearch.routes,
      ...appRouteTopic.routes,
      notFound,
    ])
  )
  httpServer.on('error', console.error)
  httpServer.listen(
    Number.parseInt(process.env.APP_PORT as string),
    process.env.APP_HOST as string,
    () => {
      if (process.env.NODE_ENV === 'development') {
        void (async () => {
          const dev = await import('@fe/api/dev.ts')
          dev.main(ctx)
        })()
      } else if (process.env.NODE_ENV !== 'development') {
        console.log(
          chalk.bold(
            `Ready at http://localhost:${
              (httpServer.address() as net.AddressInfo).port
            }/`
          )
        )
      }
    }
  )
}

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  main()
}

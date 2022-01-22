import * as appFts from '@app/api/fts.ts'
import * as appRouteFile from '@app/api/route/file.ts'
import * as appRoute from '@app/api/route/index.ts'
import * as appRouteMember from '@app/api/route/member.ts'
import * as appRouteMessage from '@app/api/route/message.ts'
import * as appRouteSearch from '@app/api/route/search.ts'
import * as appRouteTopic from '@app/api/route/topic.ts'
import * as appCacheQuery from '@app/cache/query/index.ts'
import * as appSubmit from '@app/core/submit.ts'
import * as appDbQuery from '@app/db/query/index.ts'
import * as appWidgetButton from '@app/ui/widget/button.ts'
import * as tinyRoute from '@tiny/api/route.ts'
import * as tinyRandom from '@tiny/core/random.ts'
import * as tinySubmitNode from '@tiny/core/submit.node.ts'
import type * as tinySubmit from '@tiny/core/submit.ts'
import * as tinyTimeNode from '@tiny/core/time.node.ts'
import type * as tinyTime from '@tiny/core/time.ts'
import * as tinyWidget from '@tiny/ui/widget.ts'
import chalk from 'chalk'
import jsdom from 'jsdom'
import * as nodeFs from 'node:fs'
import * as nodeHttp from 'node:http'
import type * as nodeNet from 'node:net'
import * as nodeStream from 'node:stream'
import * as nodeUrl from 'node:url'

const index = tinyRoute.defineBase(['GET'], /^\/$/, () => {
  return {
    status: 200,
    headers: {
      'content-type': 'text/html',
    },
    stream: nodeStream.Readable.toWeb(
      nodeFs.createReadStream('build/browser/index.html')
    ),
  }
})

const ssr = tinyRoute.defineBase(['GET'], /^\/ssr$/, () => {
  const dom = new jsdom.JSDOM('<!DOCTYPE html>')
  const window = dom.window
  const document = window.document
  const ctx = {
    ...tinyWidget.initContext(document),
  }
  const body = tinyWidget.toHtmlWidget(ctx, document.body)
  body.content = [appWidgetButton.custom(ctx, {})]
  return {
    status: 200,
    headers: {
      'content-type': 'text/html',
    },
    text: dom.serialize(),
  }
})

const notFound = tinyRoute.defineBase(
  undefined,
  /.*/,
  () => {
    return {
      status: 404,
      headers: {},
    }
  }
)

const js = tinyRoute.defineBase(
  ['GET'],
  /^\/js\/(?<path>.+\.mjs)$/,
  (_ctx, request) => {
    return {
      status: 200,
      headers: {
        'content-type': 'application/javascript',
      },
      stream: nodeStream.Readable.toWeb(
        nodeFs.createReadStream(
          `build/browser/js/${request.match.path}`
        )
      ),
    }
  }
)

function main(): void {
  const ctx: tinyRoute.Context &
    tinyRandom.Context &
    tinyTime.Context &
    appCacheQuery.MainContext &
    appDbQuery.ReadContext &
    appDbQuery.WriteContext &
    tinySubmit.Context &
    appFts.Context = {
    ...appRoute.initContext(),
    ...tinyRandom.initContext(),
    ...tinyTimeNode.initContext(),
    ...appCacheQuery.initContext(),
    ...appDbQuery.initContext(),
    ...tinySubmitNode.initContext(appSubmit.retryConfig),
    ...appFts.initContext(),
  }
  const httpServer = nodeHttp.createServer(
    tinyRoute.handle(ctx, [
      index,
      ssr,
      js,
      ...appRouteFile.routes,
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
          const dev = await import('@app/api/dev.ts')
          dev.main(ctx)
        })()
      } else if (process.env.NODE_ENV !== 'development') {
        console.log(
          chalk.bold(
            `Ready at http://localhost:${
              (httpServer.address() as nodeNet.AddressInfo)
                .port
            }/`
          )
        )
      }
    }
  )
}

if (
  process.argv[1] === nodeUrl.fileURLToPath(import.meta.url)
) {
  main()
}

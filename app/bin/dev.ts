import * as appRoute from '@app/api/route/index.ts'
import * as appBuild from '@app-bin/build.ts'
import * as tinyRoute from '@tiny/api/route.ts'
import chalk from 'chalk'
import chokidar from 'chokidar'
import HttpProxy from 'http-proxy'
import lodashDebounce from 'lodash-es/debounce.js'
import * as nodeFs from 'node:fs'
import * as nodeHttp from 'node:http'
import type * as nodeNet from 'node:net'
import * as nodeStream from 'node:stream'
import * as nodeUrl from 'node:url'
import WebSocket from 'ws'

import projectTsconfig from '../../tsconfig.json'

type Context = {
  sourceTree: SourceTree
  proxy: HttpProxy
} & tinyRoute.Context

type SourceTree = Record<
  string,
  Promise<appBuild.SourceFile>
>

const index = tinyRoute.defineBase(['GET'], /^\/$/, () => {
  return {
    status: 200,
    headers: {
      'content-type': 'text/html',
    },
    stream: nodeStream.Readable.toWeb(
      nodeFs.createReadStream('app/public/index.html')
    ),
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

const js = tinyRoute.defineBase<Context>(
  ['GET'],
  /^\/js\/(?<path>.+\.mjs)$/,
  async (ctx, request) => {
    const sourceFile = await Promise.resolve(
      ctx.sourceTree[
        `build/browser/js/${request.match.path}`
      ]
    )
    if (sourceFile) {
      return {
        status: 200,
        headers: {
          'content-type': 'application/javascript',
        },
        buffer: sourceFile.contents,
      }
    } else {
      return tinyRoute.forward(ctx, request, {}, notFound)
    }
  }
)

function buildDev(entryPoint: string): SourceTree {
  const sourceTree: SourceTree = {}
  const baseOptions = {
    platform: 'browser',
    define: {
      ['import.meta.env.NODE_ENV']:
        JSON.stringify('development'),
    },
    write: false,
  } as const
  loop(entryPoint)
  return sourceTree

  function loop(step: string): void {
    const options = {
      ...baseOptions,
      entryPoint: step,
    } as const
    sourceTree[appBuild.getOutputPath(options)] =
      start(step)
  }

  async function start(
    step: string
  ): Promise<appBuild.SourceFile> {
    const result = await appBuild.oneStep({
      ...baseOptions,
      entryPoint: step,
    })
    for (const step of result.nextSteps) {
      if (
        !(
          appBuild.getOutputPath({
            ...baseOptions,
            entryPoint: step,
          }) in sourceTree
        )
      ) {
        loop(step)
      }
    }
    return result.output
  }
}

async function main(): Promise<void> {
  const watcher = chokidar.watch(projectTsconfig.include, {
    ignoreInitial: true,
  })
  watcher.on(
    'all',
    lodashDebounce(() => void reload())
  )
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const entryPoint = await import.meta.resolve!(
    '@app/ui/index.ts'
  )
  const proxy = new HttpProxy({})
  proxy.on('error', () => {
    console.error('proxy error')
  })
  const ctx: Context = {
    sourceTree: buildDev(entryPoint),
    proxy,
    ...appRoute.initContext(),
  }
  const httpServer = nodeHttp.createServer(
    tinyRoute.handle(ctx, [index, js, notFound])
  )
  httpServer.on('error', console.error)
  const wsServer = new WebSocket.Server({
    server: httpServer,
  })
  wsServer.on('error', console.error)
  wsServer.on('connection', (ws, req) => {
    if (req.url !== '/api/dev') {
      ws.close()
      return
    }
    ws.on('error', console.error)
  })
  reload()
  httpServer.listen(process.env.APP_DEV_PORT)
  console.log(
    chalk.bold(
      `Ready at http://localhost:${
        (httpServer.address() as nodeNet.AddressInfo).port
      }/`
    )
  )
  return

  function reload() {
    ctx.sourceTree = buildDev(entryPoint)
    for (const ws of wsServer.clients) {
      if (ws.readyState === WebSocket.OPEN) {
        ws.send('reload')
      }
    }
  }
}

if (
  process.argv[1] === nodeUrl.fileURLToPath(import.meta.url)
) {
  void main()
}

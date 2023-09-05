import * as devBuild from '@intertwine/build_/index.ts'
import * as devRoute from '@intertwine/local/route.ts'
import chalk from 'chalk'
import chokidar from 'chokidar'
import lodashDebounce from 'lodash-es/debounce.js'
import * as nodeFs from 'node:fs'
import * as nodeHttp from 'node:http'
import type * as nodeNet from 'node:net'
import * as nodeStream from 'node:stream'
import * as nodeUrl from 'node:url'
import * as ws from 'ws'

import projectTsconfig from '../../tsconfig.json'

type Context = {
  sourceTree: SourceTree
} & devRoute.Context

type SourceTree = Record<
  string,
  Promise<devBuild.SourceFile>
>

const index = devRoute.define(['GET'], /^\/$/, () => {
  return {
    status: 200,
    headers: {
      ['content-type']: 'text/html',
    },
    stream: nodeStream.Readable.toWeb(
      nodeFs.createReadStream(
        'service/gateway/guest/public/index.html'
      )
    ),
  }
})

const notFound = devRoute.define(undefined, /.*/, () => {
  return {
    status: 404,
    headers: {},
  }
})

const js = devRoute.define<Context>(
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
          ['content-type']: 'application/javascript',
        },
        buffer: sourceFile.contents,
      }
    } else {
      return devRoute.forward(ctx, request, {}, notFound)
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
    sourceTree[devBuild.getOutputPath(options)] =
      start(step)
  }

  async function start(
    step: string
  ): Promise<devBuild.SourceFile> {
    const result = await devBuild.oneStep({
      ...baseOptions,
      entryPoint: step,
    })
    for (const step of result.nextSteps) {
      if (
        !(
          devBuild.getOutputPath({
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
    '@/index.ts'
  )
  const ctx: Context = {
    sourceTree: buildDev(entryPoint),
    maxRequestNonStreamedBytes: 4 * 1024,
  }
  const httpServer = nodeHttp.createServer(
    devRoute.handle(ctx, [index, js, notFound])
  )
  httpServer.on('error', console.error)
  const wsServer = new ws.WebSocketServer({
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

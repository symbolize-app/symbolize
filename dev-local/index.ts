import * as build from '@intertwine/dev-build'
import chalk from 'chalk'
import chokidar from 'chokidar'
import lodashDebounce from 'lodash-es/debounce.js'
import * as nodeFs from 'node:fs'
import * as nodeHttp from 'node:http'
import type * as nodeNet from 'node:net'
import * as nodeStream from 'node:stream'
import * as nodeUrl from 'node:url'
import * as ws from 'ws'
import WebSocket from 'ws'

import * as devRoute from '@/route.ts'

type Context = {
  buildResult: Promise<build.BuildResult>
} & devRoute.Context

const index = devRoute.define(['GET'], /^\/$/, () => {
  return {
    status: 200,
    headers: {
      ['content-type']: 'text/html',
    },
    stream: nodeStream.Readable.toWeb(
      nodeFs.createReadStream(
        'svc-gateway-guest/public/index.html'
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
  /^\/js\/(?<path>.+\.m?js)$/,
  async (ctx, request) => {
    const path = `/tmp/local/${request.match.path}`
    const buildResult = await ctx.buildResult
    const sourceFile = buildResult.outputFiles.find(
      (outputFile) => outputFile.path === path
    )
    if (sourceFile) {
      return {
        status: 200,
        headers: {
          ['content-type']: 'application/javascript',
          ['service-worker-allowed']: '/',
        },
        buffer: sourceFile.contents,
      }
    } else {
      return devRoute.forward(ctx, request, {}, notFound)
    }
  }
)

async function buildDev(): Promise<build.BuildResult> {
  return await build.buildCommon({
    outdir: '/tmp/local',
    nodeEnv: 'development',
    write: false,
  })
}

function main(): void {
  const watcher = chokidar.watch(
    ['./svc-auth-guest-display', './svc-gateway-guest'],
    {
      ignoreInitial: true,
    }
  )
  watcher.on(
    'all',
    lodashDebounce(() => void reload())
  )
  const ctx: Context = {
    buildResult: buildDev(),
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
    ctx.buildResult = buildDev()
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
  main()
}

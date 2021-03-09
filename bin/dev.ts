import * as build from '@fe/bin/build.ts'
import * as route from '@tiny/api/route.ts'
import chokidar from 'chokidar'
import * as fs from 'fs'
import * as http from 'http'
import debounce from 'lodash/debounce.js'
import urlModule from 'url'
import WebSocket from 'ws'

import tsconfig from '../tsconfig.json'

type Context = {
  sourceTree: SourceTree
}

type SourceTree = Record<string, Promise<build.SourceFile>>

const index = route.define(['GET'], /^\/$/, () => {
  return {
    status: 200,
    headers: {
      'content-type': 'text/html',
    },
    body: fs.createReadStream('public/index.html'),
  }
})

const notFound = route.define(undefined, /.*/, () => {
  return {
    status: 404,
    headers: {},
    body: '',
  }
})

const js = route.define<Context>(
  ['GET'],
  /^\/js\/(?<path>.+\.mjs)/,
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
        body: sourceFile.contents,
      }
    } else {
      return route.forward(ctx, request, {}, notFound)
    }
  }
)

function buildDev(
  sourceTree: SourceTree,
  entryPoint: string
): void {
  const options = {
    entryPoint,
    platform: 'browser',
    write: false,
  } as const
  sourceTree[build.getOutputPath(options)] = loop()
  return

  async function loop(): Promise<build.SourceFile> {
    const result = await build.oneStep(options)
    for (const step of result.nextSteps) {
      if (
        !(
          build.getOutputPath({
            ...options,
            entryPoint: step,
          }) in sourceTree
        )
      ) {
        buildDev(sourceTree, step)
      }
    }
    return result.output
  }
}

async function main(): Promise<void> {
  const watcher = chokidar.watch(tsconfig.include, {
    ignoreInitial: true,
  })
  watcher.on('all', debounce(reload))
  const entryPoint = await import.meta.resolve(
    '@fe/ui/index.ts'
  )
  const ctx: Context = {
    sourceTree: {},
  }
  const httpServer = http.createServer(
    route.handle(ctx, [index, js, notFound])
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
  httpServer.listen(process.env.DEV_PORT)
  return

  function reload() {
    ctx.sourceTree = {}
    buildDev(ctx.sourceTree, entryPoint)
    for (const ws of wsServer.clients) {
      if (ws.readyState === WebSocket.OPEN) {
        ws.send('reload')
      }
    }
  }
}

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void main()
}

import * as appBuild from '@fe-bin/build.ts'
import * as route from '@tiny/api/route.ts'
import * as concurrency from '@tiny/core/concurrency.ts'
import chalk from 'chalk'
import childProcess from 'child_process'
import chokidar from 'chokidar'
import * as fs from 'fs'
import * as http from 'http'
import HttpProxy from 'http-proxy'
import debounce from 'lodash-es/debounce.js'
import type * as net from 'net'
import urlModule from 'url'
import WebSocket from 'ws'

import tsconfig from '../../tsconfig.json'

type Context = {
  sourceTree: SourceTree
  proxy: HttpProxy
  server: Server
}

type SourceTree = Record<
  string,
  Promise<appBuild.SourceFile>
>

type Server = {
  ready: concurrency.EventSemaphore
  kill: () => Promise<void>
}

const index = route.define(['GET'], /^\/$/, () => {
  return {
    status: 200,
    headers: {
      'content-type': 'text/html',
    },
    stream: fs.createReadStream('app/public/index.html'),
  }
})

const notFound = route.define(undefined, /.*/, () => {
  return {
    status: 404,
    headers: {},
  }
})

const js = route.define<Context>(
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
      return route.forward(ctx, request, {}, notFound)
    }
  }
)

const api = route.define<{
  server: Server
  proxy: HttpProxy
}>(undefined, /^\/api\/.+/, async (ctx) => {
  await ctx.server.ready.wait()
  return (req, res) =>
    ctx.proxy.web(req, res, {
      target: `http://localhost:${
        process.env.APP_PORT as string
      }`,
    })
})

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

function createServer(): Server {
  const ready = new concurrency.EventSemaphore()
  const exited = new concurrency.EventSemaphore()
  const child = childProcess.fork('app/src/api/index.ts', {
    env: {
      ...process.env,
      ['NODE_ENV']: 'development',
    },
  })
  child.on('message', (message) => {
    if (message === 'ready') {
      console.log(chalk.blue('dev server ready'))
      ready.set()
    }
  })
  child.on('error', console.error)
  child.on('exit', () => {
    ready.clear()
    exited.set()
  })
  return { ready, kill }

  async function kill() {
    child.kill()
    await exited.wait()
  }
}

async function main(): Promise<void> {
  const watcher = chokidar.watch(tsconfig.include, {
    ignoreInitial: true,
  })
  watcher.on(
    'all',
    debounce(() => void reload())
  )
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const entryPoint = await import.meta.resolve!(
    '@fe/ui/index.ts'
  )
  const proxy = new HttpProxy({})
  proxy.on('error', () => {
    console.error('proxy error')
  })
  const ctx: Context = {
    sourceTree: buildDev(entryPoint),
    proxy,
    server: createServer(),
  }
  const httpServer = http.createServer(
    route.handle(ctx, [index, js, api, notFound])
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
  await reload()
  httpServer.listen(process.env.APP_DEV_PORT)
  console.log(
    chalk.bold(
      `Ready at http://localhost:${
        (httpServer.address() as net.AddressInfo).port
      }/`
    )
  )
  return

  async function reload() {
    ctx.sourceTree = buildDev(entryPoint)
    await ctx.server.kill()
    ctx.server = createServer()
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

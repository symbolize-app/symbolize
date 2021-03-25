import * as message from '@fe/core/message.ts'
import type * as db from '@fe/db/index.ts'
import * as memberQuery from '@fe/db/query/member.ts'
import * as button from '@fe/ui/button.ts'
import * as route from '@tiny/api/route.ts'
import * as widget from '@tiny/ui/widget.ts'
import chalk from 'chalk'
import * as fs from 'fs'
import * as http from 'http'
import jsdom from 'jsdom'
import type * as net from 'net'
import pg from 'pg'
import pgConnectionString from 'pg-connection-string'
import urlModule from 'url'

type Context = {
  databaseApiRead: db.DatabaseApiRead
}

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
  const document = dom.window.document
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

const apiMessage = route.define<{
  databaseApiRead: db.DatabaseApiRead
}>(['GET'], /^\/api\/message$/, async (ctx) => {
  const rows = await memberQuery.find(
    ctx.databaseApiRead,
    Buffer.from('ABCD', 'hex')
  )

  return {
    status: 200,
    headers: {
      'content-type': 'text/plain',
    },
    body: `${message.hi} ${JSON.stringify(rows)}`,
  }
})

async function main(): Promise<void> {
  const ctx: Context = {
    databaseApiRead: {
      pool: openPool(
        process.env.DATABASE_URL_API_READ as string
      ),
    } as db.DatabaseApiRead,
  }
  const httpServer = http.createServer(
    route.handle(ctx, [
      index,
      ssr,
      js,
      apiMessage,
      notFound,
    ])
  )
  httpServer.on('error', console.error)
  httpServer.listen(process.env.PORT)

  if (process.env.NODE_ENV === 'development') {
    const dev = await import('@fe/api/dev.ts')
    dev.main()
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

function openPool(connectionString: string): pg.Pool {
  const max = 10
  const user = pgConnectionString.parse(connectionString)
    .user
  if (!user) {
    throw new Error('No DB user')
  }

  const pool = new pg.Pool({
    connectionString,
    connectionTimeoutMillis: 1_000,
    idleTimeoutMillis: 10_000,
    max,
  })
  pool.on('error', console.error)
  pool.on('connect', () => {
    console.log(
      chalk.magenta(
        `[${user}] DB client connected (${pool.totalCount} / ${max})`
      )
    )
  })
  pool.on('remove', () => {
    console.log(
      chalk.magenta(
        `[${user}] DB client removed (${pool.totalCount} / ${max})`
      )
    )
  })
  return pool
}

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void main()
}

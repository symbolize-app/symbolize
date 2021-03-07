import * as http from 'http'
import * as stream from 'stream'
import * as streamPromises from 'stream/promises'
import type * as typeFest from 'type-fest'

export type Request = {
  url: URL
  match: Record<string, string>
  method: string
  headers: Record<string, string>
  stream: stream.Readable
}

export async function readRequestBuffer(
  request: Request
): Promise<Buffer> {
  return new Promise((resolve, reject) => {
    const buffer = Buffer.alloc(0)
    request.stream.on('data', (chunk) => {
      buffer.write(chunk)
    })
    request.stream.on('end', () => {
      resolve(buffer)
    })
    request.stream.on('error', reject)
  })
}

export async function readRequestText(
  request: Request
): Promise<string> {
  return (await readRequestBuffer(request)).toString()
}

export async function readRequestObject(
  request: Request
): Promise<typeFest.JsonObject> {
  const result: unknown = JSON.parse(
    await readRequestText(request)
  )
  if (typeof result === 'object' && result !== null) {
    return result
  } else {
    throw Error('Invalid type')
  }
}

export type Response = typeFest.Promisable<{
  status: number
  headers: Record<string, string>
  body: typeFest.Promisable<
    | string
    | Buffer
    | Uint8Array
    | stream.Readable
    | typeFest.JsonObject
  >
}>

export type Handler = (request: Request) => Response

export type Route = {
  methods: string[] | undefined
  match: RegExp
  handler: Handler
}

export function define(
  methods: string[] | undefined,
  match: RegExp,
  handler: Handler
): Route {
  return { methods, match, handler }
}

export function handle(
  routes: Route[]
): http.RequestListener {
  return (req, res) => {
    void handleRequest(routes, req, res)
  }
}

async function handleRequest(
  routes: Route[],
  req: http.IncomingMessage,
  res: http.ServerResponse
): Promise<void> {
  if (!req.method) {
    console.error('Missing method')
    return
  }
  if (!req.url) {
    console.error('Missing URL')
    return
  }
  req.on('error', console.error)
  res.on('error', console.error)
  try {
    let response: Response | undefined = undefined
    for (const route of routes) {
      const match =
        (route.methods === undefined ||
          route.methods.indexOf(req.method) >= 0) &&
        route.match.exec(req.url)
      if (match) {
        response = route.handler({
          url: new URL(
            req.url,
            `http://${req.headers.host || 'localhost'}`
          ),
          match: match.groups ?? {},
          method: req.method,
          headers: Object.fromEntries(
            Object.entries(req.headers).filter(
              ([_key, value]) => typeof value === 'string'
            ) as [string, string][]
          ),
          stream: req,
        })
        break
      }
    }
    if (!response) {
      throw new Error('No route found')
    }
    const headResponse = await Promise.resolve(response)
    res.writeHead(headResponse.status, headResponse.headers)
    const body = await Promise.resolve(headResponse.body)
    if (typeof body === 'string') {
      res.write(body)
    } else if (body instanceof Buffer) {
      res.write(body)
    } else if (body instanceof Uint8Array) {
      res.write(body)
    } else if (body instanceof stream.Readable) {
      await streamPromises.pipeline(body, res)
    } else {
      const result = JSON.stringify(body)
      res.write(result)
    }
  } catch (error) {
    console.error(error)
    if (res.headersSent) {
      console.error('Headers already sent')
    } else {
      res.writeHead(500)
    }
  }
  res.end()
}

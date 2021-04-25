import type * as http from 'http'
import * as stream from 'stream'
import * as streamPromises from 'stream/promises'
import type * as typeFest from 'type-fest'

export type Request = {
  origin: string
  path: string
  match: Record<string, string>
  params: Record<string, string>
  method: string
  headers: Record<string, string>
  stream(): stream.Readable
  buffer(): Promise<Buffer>
  text(): Promise<string>
  json(): Promise<typeFest.JsonObject>
}

export type Response = typeFest.Promisable<
  | {
      status: number
      headers: Record<string, string>
      body?: typeFest.Promisable<
        | undefined
        | string
        | Buffer
        | Uint8Array
        | stream.Readable
        | typeFest.JsonObject
      >
    }
  | http.RequestListener
>

export class ResponseError extends Error {
  response: {
    status: number
    headers: Record<string, string>
    body?:
      | undefined
      | string
      | Buffer
      | Uint8Array
      | stream.Readable
      | typeFest.JsonObject
  }

  constructor(response: ResponseError['response']) {
    super(`Response ${response.status} error`)
    this.response = response
  }
}

export type Handler<Context> = (
  ctx: Context,
  request: Request
) => Response

export type Route<Context> = {
  methods: string[] | undefined
  match: RegExp
  handler: Handler<Context>
}

export function define<Context = unknown>(
  methods: string[] | undefined,
  match: RegExp,
  handler: Handler<Context>
): Route<Context> {
  return { methods, match, handler }
}

export function handle<Context>(
  ctx: Context,
  routes: Route<Context>[]
): http.RequestListener {
  return (req, res) => {
    void handleRequest(ctx, routes, req, res)
  }
}

async function handleRequest<Context>(
  ctx: Context,
  routes: Route<Context>[],
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
    const url = new URL(
      req.url,
      `http://${req.headers.host || 'localhost'}`
    )
    const request: Request = {
      origin: url.origin,
      path: url.pathname,
      params: Object.fromEntries(
        url.searchParams.entries()
      ),
      match: {},
      method: req.method,
      headers: Object.fromEntries(
        Object.entries(req.headers).filter(
          ([_key, value]) => typeof value === 'string'
        ) as [string, string][]
      ),
      stream() {
        return req
      },
      async buffer() {
        return new Promise((resolve, reject) => {
          const chunks: Buffer[] = []
          request.stream().on('data', (chunk) => {
            chunks.push(chunk)
          })
          request.stream().on('end', () => {
            resolve(Buffer.concat(chunks))
          })
          request.stream().on('error', reject)
        })
      },
      async text() {
        return (await request.buffer()).toString()
      },
      async json() {
        const result: unknown = JSON.parse(
          await request.text()
        )
        if (typeof result === 'object' && result !== null) {
          return result
        } else {
          throw Error('Invalid type')
        }
      },
    }
    const response = match(ctx, request, routes)
    let headResponse: typeFest.PromiseValue<Response>
    try {
      headResponse = await Promise.resolve(response)
    } catch (error: unknown) {
      if (error instanceof ResponseError) {
        headResponse = error.response
      } else {
        throw error
      }
    }
    if (typeof headResponse === 'function') {
      headResponse(req, res)
    } else {
      res.writeHead(
        headResponse.status,
        headResponse.headers
      )
      const body = await Promise.resolve(headResponse.body)
      if (typeof body === undefined) {
        // No body
      } else if (typeof body === 'string') {
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
      res.end()
    }
  } catch (error: unknown) {
    console.error(error)
    if (res.headersSent) {
      console.error('Headers already sent')
    } else {
      res.writeHead(500)
    }
    res.end()
  }
}

export function match<Context>(
  ctx: Context,
  request: Request,
  routes: Route<Context>[]
): Response {
  let response: Response | undefined = undefined
  for (const route of routes) {
    const match =
      (route.methods === undefined ||
        route.methods.indexOf(request.method) >= 0) &&
      route.match.exec(request.path)
    if (match) {
      response = route.handler(ctx, {
        ...request,
        match: match.groups ?? {},
      })
      break
    }
  }
  if (!response) {
    throw new Error('No route found')
  }
  return response
}

export function forward<Context>(
  ctx: Context,
  request: Request,
  match: Record<string, string>,
  route: Route<Context>
): Response {
  return route.handler(ctx, {
    ...request,
    match,
  })
}

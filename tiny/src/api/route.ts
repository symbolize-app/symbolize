import type * as endpoint from '@tiny/core/endpoint.ts'
import * as formData from 'formdata-polyfill/esm.min.js'
import type * as http from 'http'
import * as nodeFetch from 'node-fetch'
import * as streamPromises from 'stream/promises'
import type * as typeFest from 'type-fest'

export type Request = {
  origin: string
  path: string
  match: Record<string, string>
  params: Record<string, string>
  method: string
  headers: Record<string, string>
  stream(): NodeJS.ReadableStream
  buffer(): Promise<ArrayBuffer>
  text(): Promise<string>
  form(): Promise<FormData>
  json(): Promise<typeFest.JsonValue>
}

export type Response = typeFest.Promisable<
  | {
      status: number
      headers?: Record<string, string>
      stream?: typeFest.Promisable<NodeJS.ReadableStream>
      buffer?: typeFest.Promisable<ArrayBuffer>
      text?: typeFest.Promisable<string>
      form?: typeFest.Promisable<FormData>
      json?: typeFest.Promisable<typeFest.JsonValue>
    }
  | http.RequestListener
>

export class ResponseError extends Error {
  response: {
    status: number
    headers?: Record<string, string>
    buffer?: ArrayBuffer
    text?: string
    form?: FormData
    json?: typeFest.JsonValue
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
  match: string | RegExp
  handler: Handler<Context>
}

export function define<Context = unknown>(
  methods: string[] | undefined,
  match: string | RegExp,
  handler: Handler<Context>
): Route<Context> {
  return { methods, match, handler }
}

export function defineEndpoint<Context = unknown>(
  endpoint: endpoint.BaseEndpoint,
  handler: Handler<Context>
): Route<Context> {
  return {
    methods: [endpoint.method],
    match: endpoint.path,
    handler,
  }
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
        return await new nodeFetch.BodyMixin(
          req
        ).arrayBuffer()
      },
      async text() {
        return (await request.buffer()).toString()
      },
      async form() {
        return await new nodeFetch.BodyMixin(req).formData()
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
    let headResponse: Awaited<Response>
    try {
      headResponse = await Promise.resolve(response)
    } catch (error) {
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
      if (headResponse.stream !== undefined) {
        await streamPromises.pipeline(
          await Promise.resolve(headResponse.stream),
          res
        )
      } else if (headResponse.buffer !== undefined) {
        res.write(
          await Promise.resolve(headResponse.buffer)
        )
      } else if (headResponse.text !== undefined) {
        res.write(await Promise.resolve(headResponse.text))
      } else if (headResponse.form !== undefined) {
        const result = formData.formDataToBlob(
          await Promise.resolve(headResponse.form)
        )
        res.write(result)
      } else if (headResponse.json !== undefined) {
        const result = JSON.stringify(
          await Promise.resolve(headResponse.json)
        )
        res.write(result)
      }
      res.end()
    }
  } catch (error) {
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
      (typeof route.match === 'string'
        ? route.match === request.path && {
            groups: undefined,
          }
        : route.match.exec(request.path))
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

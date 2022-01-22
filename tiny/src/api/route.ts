import type * as tinyEndpoint from '@tiny/core/endpoint.ts'
import * as tinyPayload from '@tiny/core/payload.ts'
import * as formDataPolyfill from 'formdata-polyfill/esm.min.js'
import type * as nodeHttp from 'node:http'
import * as nodeStream from 'node:stream'
import * as nodeStreamPromises from 'node:stream/promises'
import nodeFetchBody from 'node-fetch/src/body.js'
import type * as typeFest from 'type-fest'

export type Context = {
  maxRequestNonStreamedBytes: number
}

export type Request = {
  origin: string
  path: string
  match: Record<string, string>
  params: Record<string, string>
  method: string
  headers: Record<string, string>
  stream(): ReadableStream
  blob(): Promise<Blob>
  buffer(): Promise<ArrayBuffer>
  text(): Promise<string>
  form(): Promise<FormData>
  json(): Promise<typeFest.JsonValue>
}

export type Response = typeFest.Promisable<
  | {
      status: number
      headers?: Record<string, string>
      stream?: typeFest.Promisable<ReadableStream>
      blob?: typeFest.Promisable<Blob>
      buffer?: typeFest.Promisable<ArrayBuffer>
      text?: typeFest.Promisable<string>
      form?: typeFest.Promisable<FormData>
      json?: typeFest.Promisable<typeFest.JsonValue>
    }
  | nodeHttp.RequestListener
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

export type Handler<
  CustomContext,
  CustomRequest = Request,
  CustomResponse = Response
> = (
  ctx: CustomContext,
  request: CustomRequest
) => CustomResponse

export type Route<CustomContext> = {
  methods: string[] | undefined
  match: string | RegExp
  handler: Handler<CustomContext>
}

export function defineBase<CustomContext = unknown>(
  methods: string[] | undefined,
  match: string | RegExp,
  handler: Handler<CustomContext>
): Route<CustomContext> {
  return { methods, match, handler }
}

export function define<
  Endpoint extends tinyEndpoint.BaseEndpoint,
  CustomContext = unknown
>(
  endpoint: Endpoint,
  handler: Handler<
    CustomContext,
    Pick<
      Request,
      'origin' | 'path' | 'method' | 'headers'
    > &
      (Endpoint extends tinyEndpoint.RequestParamsEndpoint
        ? {
            params: tinyPayload.Payload<
              Endpoint['requestParams']
            >
          }
        : { params?: never }) &
      (Endpoint extends tinyEndpoint.RequestBytesEndpoint
        ? Pick<Request, 'stream' | 'blob' | 'buffer'>
        : {
            stream?: never
            blob?: never
            buffer?: never
          }) &
      (Endpoint extends tinyEndpoint.RequestJsonEndpoint
        ? {
            json: tinyPayload.Payload<
              Endpoint['requestJson']
            >
          }
        : { json?: never }),
    typeFest.Promisable<
      {
        status: 200
        headers?: Record<string, string>
      } & (Endpoint extends tinyEndpoint.OkResponseStreamEndpoint
        ? {
            stream?: typeFest.Promisable<ReadableStream>
            blob?: typeFest.Promisable<Blob>
            buffer?: typeFest.Promisable<ArrayBuffer>
          }
        : { stream?: never }) &
        (Endpoint extends tinyEndpoint.OkResponseJsonEndpoint
          ? {
              json: typeFest.Promisable<
                tinyPayload.Payload<
                  Endpoint['okResponseJson']
                >
              >
            }
          : { json?: never })
    >
  >
): Route<CustomContext> {
  return defineBase(
    [endpoint.method],
    endpoint.path,
    async (ctx, request) => {
      const response = await checkConflictQuery(
        endpoint,
        async () =>
          Promise.resolve(
            handler(ctx, {
              ...request,
              ...(endpoint.requestParams !== undefined
                ? {
                    params: checkRequestParams(
                      endpoint as tinyEndpoint.RequestParamsEndpoint,
                      request
                    ),
                  }
                : undefined),
              ...(endpoint.requestJson !== undefined
                ? {
                    json: await checkRequestJson(
                      endpoint as tinyEndpoint.RequestJsonEndpoint,
                      request
                    ),
                  }
                : undefined),
            } as never)
          )
      )
      return {
        ...response,
        headers: {
          ...(endpoint.okResponseJson !== undefined && {
            'content-type': 'application/json',
          }),
          ...response.headers,
        },
        json:
          endpoint.okResponseJson !== undefined
            ? checkResponseJson(
                endpoint as tinyEndpoint.OkResponseJsonEndpoint,
                response as never
              )
            : undefined,
      }
    }
  )
}

function checkRequestParams<
  Value extends Record<string, string>
>(
  endpoint: {
    requestParams: tinyPayload.StringValidator<Value>
  },
  request: Request
): Value {
  const input = request.params
  return checkRequestBase(endpoint.requestParams, input)
}

export async function checkRequestJson<
  Value extends typeFest.JsonValue
>(
  endpoint: {
    requestJson: tinyPayload.Validator<Value>
  },
  request: Request
): Promise<Value> {
  if (
    request.headers['content-type'] !== 'application/json'
  ) {
    throw new ResponseError({
      status: 400,
      headers: {
        'content-type': 'application/json',
      },
      json: {
        error: 'content-type: application/json required',
      },
    })
  }
  let input: typeFest.JsonValue
  try {
    input = await request.json()
  } catch (error) {
    if (error instanceof SyntaxError) {
      throw new ResponseError({
        status: 400,
        headers: {
          'content-type': 'application/json',
        },
        json: {
          error: 'JSON syntax error',
        },
      })
    } else {
      throw error
    }
  }
  return checkRequestBase(endpoint.requestJson, input)
}

function checkRequestBase<Value>(
  validator: tinyPayload.Validator<Value>,
  input: typeFest.JsonValue
): Value {
  try {
    return validator.check(input)
  } catch (error) {
    if (error instanceof tinyPayload.PayloadError) {
      throw new ResponseError({
        status: 400,
        headers: {
          'content-type': 'application/json',
        },
        json: {
          error: error.message,
        },
      })
    } else {
      throw error
    }
  }
}

async function checkConflictQuery<Value>(
  endpoint: tinyEndpoint.BaseEndpoint,
  query: () => Promise<Value>
): Promise<Value> {
  try {
    return await query()
  } catch (error) {
    if (
      endpoint.conflictResponseJson !== undefined &&
      error instanceof tinyPayload.ConflictError
    ) {
      throw createConflictResponseError(
        endpoint as never,
        error.field as never
      )
    } else {
      throw error
    }
  }
}

function createConflictResponseError<
  Endpoint extends {
    conflictResponseJson: tinyPayload.ConflictValidator
  }
>(
  endpoint: Endpoint,
  field: tinyPayload.Payload<
    Endpoint['conflictResponseJson']
  >['conflict']
): ResponseError {
  return new ResponseError({
    status: 409,
    headers: {
      'content-type': 'application/json',
    },
    json: endpoint.conflictResponseJson.check({
      conflict: field,
    }),
  })
}

function checkResponseJson<
  Value extends typeFest.JsonValue
>(
  endpoint: {
    okResponseJson: tinyPayload.Validator<Value>
  },
  response: {
    json: typeFest.Promisable<Value>
  }
): typeFest.Promisable<Value> {
  if (
    (response.json as unknown as PromiseLike<Value>)[
      'then'
    ] instanceof Function
  ) {
    return (
      response.json as unknown as PromiseLike<Value>
    ).then(endpoint.okResponseJson.check)
  } else {
    return endpoint.okResponseJson.check(
      response.json as Value
    )
  }
}

export function handle<CustomContext>(
  ctx: CustomContext & Context,
  routes: Route<CustomContext>[]
): nodeHttp.RequestListener {
  return (req, res) => {
    void handleRequest(ctx, routes, req, res)
  }
}

async function handleRequest<CustomContext>(
  ctx: CustomContext & Context,
  routes: Route<CustomContext>[],
  req: nodeHttp.IncomingMessage,
  res: nodeHttp.ServerResponse
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
    const checkContentLength = () => {
      if (
        !(
          Number.parseInt(
            req.headers['content-length'] ?? ''
          ) <= ctx.maxRequestNonStreamedBytes
        )
      ) {
        throw new ResponseError({
          status: 400,
          text: `invalid content length (max ${ctx.maxRequestNonStreamedBytes} bytes)`,
        })
      }
    }
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
        return nodeStream.Readable.toWeb(req)
      },
      async blob() {
        checkContentLength()
        return await new nodeFetchBody(req).blob()
      },
      async buffer() {
        checkContentLength()
        return await new nodeFetchBody(req).arrayBuffer()
      },
      async text() {
        checkContentLength()
        return await new nodeFetchBody(req).text()
      },
      async form() {
        checkContentLength()
        return await new nodeFetchBody(req).formData()
      },
      async json() {
        checkContentLength()
        return (await new nodeFetchBody(
          req
        ).json()) as typeFest.JsonValue
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
        await nodeStreamPromises.pipeline(
          nodeStream.Readable.fromWeb(
            await Promise.resolve(headResponse.stream)
          ),
          res
        )
      } else if (headResponse.blob !== undefined) {
        await nodeStreamPromises.pipeline(
          (
            await Promise.resolve(headResponse.blob)
          ).stream(),
          res
        )
      } else if (headResponse.buffer !== undefined) {
        res.write(
          await Promise.resolve(headResponse.buffer)
        )
      } else if (headResponse.text !== undefined) {
        res.write(await Promise.resolve(headResponse.text))
      } else if (headResponse.form !== undefined) {
        const result = formDataPolyfill.formDataToBlob(
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

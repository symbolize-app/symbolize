import type * as endpoint from '@tiny/core/endpoint.ts'
import * as errorModule from '@tiny/core/error.ts'
import type * as payload from '@tiny/core/payload.ts'
import * as time from '@tiny/core/time.ts'
import ms from 'ms'
import type * as typeFest from 'type-fest'

export type Request = {
  origin?: string
  path: string
  params?: Record<string, string>
  method: string
  headers?: Record<string, string>
  stream?: ReadableStream
  buffer?: ArrayBuffer
  blob?: Blob
  text?: string
  form?: FormData
  json?: typeFest.JsonValue
}

export type Response = {
  status: number
  headers: Record<string, string>
  stream(): ReadableStream
  buffer(): Promise<ArrayBuffer>
  blob(): Promise<Blob>
  text(): Promise<string>
  form(): Promise<FormData>
  json(): Promise<typeFest.JsonValue>
}

export type Context = {
  submit(request: Request): Promise<Response>
}

export function initContext(window: {
  fetch(
    input: Exclude<
      Parameters<Window['fetch']>['0'],
      Request
    >,
    init?: Parameters<Window['fetch']>['1']
  ): ReturnType<Window['fetch']>
}): Context {
  return {
    async submit(request) {
      let body =
        request.stream ??
        request.blob ??
        request.buffer ??
        request.text ??
        request.form
      if (request.json !== undefined) {
        body = JSON.stringify(request.json)
      }
      const response = await window.fetch(getUrl(request), {
        method: request.method,
        headers: request.headers,
        body: body,
      })
      return {
        status: response.status,
        headers: Object.fromEntries(
          response.headers.entries()
        ),
        stream() {
          if (!response.body) {
            throw new Error('No response stream')
          }
          return response.body
        },
        blob() {
          return response.blob()
        },
        buffer() {
          return response.arrayBuffer()
        },
        text() {
          return response.text()
        },
        form() {
          return response.formData()
        },
        json() {
          return response.json()
        },
      }
    },
  }
}

export function getUrl(request: Request): string {
  let url = request.path
  if (request.origin) {
    url = new URL(url, request.origin).toString()
  }
  if (request.params) {
    const params = new URLSearchParams(
      request.params
    ).toString()
    if (params) {
      url = `${url}?${params}`
    }
  }
  return url
}

export function retrySubmit<
  Endpoint extends endpoint.BaseEndpoint
>(
  ctx: errorModule.Context & Context,
  description: string,
  endpoint: Endpoint,
  request: Pick<Request, 'origin' | 'headers'> &
    (Endpoint extends endpoint.RequestParamsEndpoint
      ? {
          params: payload.Payload<Endpoint['requestParams']>
        }
      : { params?: never }) &
    (Endpoint extends endpoint.RequestBytesEndpoint
      ? {
          stream?: ReadableStream
          blob?: Blob
          buffer?: ArrayBuffer
        }
      : { stream?: never; blob?: never; buffer?: never }) &
    (Endpoint extends endpoint.RequestJsonEndpoint
      ? {
          json: payload.Payload<Endpoint['requestJson']>
        }
      : { json?: never })
): Promise<
  (Endpoint extends endpoint.OkResponseStreamEndpoint
    ? {
        stream: ReadableStream
      }
    : { stream?: never }) &
    (Endpoint extends endpoint.OkResponseJsonEndpoint
      ? {
          json: payload.Payload<Endpoint['okResponseJson']>
        }
      : { json?: never })
> {
  const checkedRequest = createRequest(endpoint, request)
  return errorModule.retry(
    ctx,
    async () => {
      const response = await ctx.submit(checkedRequest)
      if (endpoint.conflictResponseJson) {
        if (response.status === 409) {
          const conflictResponseData =
            endpoint.conflictResponseJson.check(
              await response.json()
            )
          throw new endpoint.conflictResponseJson.error(
            conflictResponseData.conflict as never
          )
        }
      }
      if (response.status === 200) {
        return {
          ...(endpoint.okResponseStream !== undefined && {
            stream: response.stream(),
          }),
          ...(endpoint.okResponseJson !== undefined && {
            json: endpoint.okResponseJson.check(
              await response.json()
            ),
          }),
        } as (Endpoint extends endpoint.OkResponseStreamEndpoint
          ? {
              stream: ReadableStream
            }
          : { stream?: never }) &
          (Endpoint extends endpoint.OkResponseJsonEndpoint
            ? {
                json: payload.Payload<
                  Endpoint['okResponseJson']
                >
              }
            : { json?: never })
      } else {
        throw new Error(
          `Unexpected status ${response.status} response during ${description}`
        )
      }
    },
    {
      maxAttempts: 15,
      minDelayMs: time.interval({
        milliseconds: 10,
      }),
      windowMs: time.interval({ seconds: 30 }),
      onError(error, attempt, nextDelayMs) {
        if (endpoint.conflictResponseJson) {
          if (
            error instanceof
            endpoint.conflictResponseJson.error
          ) {
            throw error
          }
        }
        console.error(
          `Retrying ${description} submit (attempt ${attempt}, delay ${ms(
            nextDelayMs
          )})`,
          error
        )
      },
    }
  )
}

export function getEndpointUrl<
  Endpoint extends endpoint.BaseEndpoint
>(
  endpoint: Endpoint,
  request: Pick<Request, 'origin' | 'headers'> &
    (Endpoint extends endpoint.RequestParamsEndpoint
      ? {
          params: payload.Payload<Endpoint['requestParams']>
        }
      : { params?: never }) &
    (Endpoint extends endpoint.RequestBytesEndpoint
      ? {
          stream?: ReadableStream
          blob?: Blob
          buffer?: ArrayBuffer
        }
      : { stream?: never; blob?: never; buffer?: never }) &
    (Endpoint extends endpoint.RequestJsonEndpoint
      ? {
          json: payload.Payload<Endpoint['requestJson']>
        }
      : { json?: never })
): string {
  return getUrl(createRequest(endpoint, request))
}

export function createRequest<
  Endpoint extends endpoint.BaseEndpoint
>(
  endpoint: Endpoint,
  request: Pick<Request, 'origin' | 'headers'> &
    (Endpoint extends endpoint.RequestParamsEndpoint
      ? {
          params: payload.Payload<Endpoint['requestParams']>
        }
      : { params?: never }) &
    (Endpoint extends endpoint.RequestBytesEndpoint
      ? {
          stream?: ReadableStream
          blob?: Blob
          buffer?: ArrayBuffer
        }
      : { stream?: never; blob?: never; buffer?: never }) &
    (Endpoint extends endpoint.RequestJsonEndpoint
      ? {
          json: payload.Payload<Endpoint['requestJson']>
        }
      : { json?: never })
): Request {
  return {
    ...(request.origin !== undefined && {
      origin: request.origin,
    }),
    path: endpoint.path,
    method: endpoint.method,
    headers: {
      ...(endpoint.requestJson !== undefined && {
        ['content-type']: 'application/json',
      }),
      ...request.headers,
    },
    ...(endpoint.requestParams !== undefined && {
      params: endpoint.requestParams.check(
        request.params as unknown as Record<string, string>
      ),
    }),
    ...(endpoint.requestBytes !== undefined && {
      stream: request.stream,
      blob: request.blob,
      buffer: request.buffer,
    }),
    ...(endpoint.requestJson !== undefined && {
      json: endpoint.requestJson.check(
        request.json as unknown as typeFest.JsonValue
      ),
    }),
  }
}

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

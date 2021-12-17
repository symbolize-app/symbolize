import type * as typeFest from 'type-fest'

export type Request = {
  origin?: string
  path: string
  params?: Record<string, string>
  method: string
  headers?: Record<string, string>
  body?:
    | undefined
    | string
    | ArrayBuffer
    | Uint8Array
    | FormData
    | typeFest.JsonObject
}

export type Response = {
  status: number
  headers: Record<string, string>
  stream(): ReadableStream
  buffer(): Promise<ArrayBuffer>
  text(): Promise<string>
  json(): Promise<typeFest.JsonObject>
}

export type Context = {
  submit(request: Request): Promise<Response>
}

export function initContext(
  window: Pick<
    Window & typeof globalThis,
    'fetch' | 'FormData'
  >
): Context {
  return {
    async submit(request) {
      let body
      if (
        request.body === undefined ||
        typeof request.body === 'string' ||
        request.body instanceof ArrayBuffer ||
        request.body instanceof Uint8Array ||
        request.body instanceof window.FormData
      ) {
        body = request.body
      } else {
        body = JSON.stringify(request.body)
      }
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
      const response = await window.fetch(url, {
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
        buffer() {
          return response.arrayBuffer()
        },
        text() {
          return response.text()
        },
        json() {
          return response.json()
        },
      }
    },
  }
}

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
  window: Window & typeof globalThis
): Context {
  return {
    async submit(request) {
      let windowBody
      if (
        request.body === undefined ||
        typeof request.body === 'string' ||
        request.body instanceof ArrayBuffer ||
        request.body instanceof Uint8Array ||
        request.body instanceof window.FormData
      ) {
        windowBody = request.body
      } else {
        windowBody = JSON.stringify(request.body)
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
      const windowResponse = await window.fetch(url, {
        method: request.method,
        headers: request.headers,
        body: windowBody,
      })
      return {
        status: windowResponse.status,
        headers: Object.fromEntries(
          windowResponse.headers.entries()
        ),
        stream() {
          if (!windowResponse.body) {
            throw new Error('No response stream')
          }
          return windowResponse.body
        },
        buffer() {
          return windowResponse.arrayBuffer()
        },
        text() {
          return windowResponse.text()
        },
        json() {
          return windowResponse.json()
        },
      }
    },
  }
}

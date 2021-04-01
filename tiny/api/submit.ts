import type * as typeFest from 'type-fest'

export type Request = {
  url: string
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
  buffer(): Promise<ArrayBuffer>
  text(): Promise<string>
  json(): Promise<typeFest.JsonObject>
}

export type SubmitContext = {
  submit(request: Request): Promise<Response>
}

export function initContext(
  window: Window & typeof globalThis
): SubmitContext {
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
      const windowResponse = await window.fetch(
        request.url,
        {
          method: request.method,
          headers: request.headers,
          body: windowBody,
        }
      )
      return {
        status: windowResponse.status,
        headers: Object.fromEntries(
          windowResponse.headers.entries()
        ),
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

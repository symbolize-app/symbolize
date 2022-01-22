import type * as tinyError from '@tiny/core/error.ts'
import * as tinySubmit from '@tiny/core/submit.ts'
import * as nodeStream from 'node:stream'
import * as nodeWebStream from 'node:stream/web'
import * as nodeFetch from 'node-fetch'

class ConvertedResponse {
  #response
  constructor(response: nodeFetch.Response) {
    this.#response = response
  }
  get headers(): Headers {
    return this.#response.headers
  }
  get ok(): boolean {
    return this.#response.ok
  }
  get redirected(): boolean {
    return this.#response.redirected
  }
  get status(): number {
    return this.#response.status
  }
  get statusText(): string {
    return this.#response.statusText
  }
  get type(): ResponseType {
    return this.#response.type
  }
  get url(): string {
    return this.#response.url
  }
  get body(): ReadableStream<Uint8Array> | null {
    return (
      this.#response.body &&
      nodeStream.Readable.toWeb(this.#response.body)
    )
  }
  get bodyUsed(): boolean {
    return this.#response.bodyUsed
  }
  clone(): Response {
    return new ConvertedResponse(this.#response.clone())
  }
  arrayBuffer(): Promise<ArrayBuffer> {
    return this.#response.arrayBuffer()
  }
  blob(): Promise<Blob> {
    return this.#response.blob()
  }
  formData(): Promise<FormData> {
    return this.#response.formData()
  }
  json(): Promise<unknown> {
    return this.#response.json()
  }
  text(): Promise<string> {
    return this.#response.text()
  }
}

export function initContext(
  submitRetryConfig: Omit<tinyError.RetryConfig, 'onError'>
): tinySubmit.Context {
  return tinySubmit.initContext(
    {
      async fetch(
        input: string,
        init?: RequestInit
      ): Promise<Response> {
        const nodeInput = input
        const nodeInit = init && {
          ...init,
          body:
            init.body instanceof
            nodeWebStream.ReadableStream
              ? nodeStream.Readable.fromWeb(init.body)
              : (init.body as nodeFetch.RequestInit['body']),
        }
        const nodeResponse = await nodeFetch.default(
          nodeInput,
          nodeInit
        )
        return new ConvertedResponse(nodeResponse)
      },
    },
    submitRetryConfig
  )
}

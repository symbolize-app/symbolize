/* eslint-disable @typescript-eslint/naming-convention */

declare module '*.sql' {
  const text: string
  // eslint-disable-next-line import/no-default-export
  export default text
}

declare module 'fast-mersenne-twister' {
  export function MersenneTwister(
    seed: number | number[]
  ): {
    randomNumber(): number
    genrand_int32(): number
    random31Bit(): number
    genrand_int31(): number
    randomInclusive(): number
    genrand_real1(): number
    random(): number
    genrand_real2(): number
    randomExclusive(): number
    genrand_real3(): number
    random53Bit(): number
    genrand_res53(): number
  }
}

declare module 'stream' {
  export namespace Readable {
    function toWeb(
      streamReadable: NodeJS.ReadableStream
    ): ReadableStream
    function fromWeb(
      readableStream: ReadableStream,
      options?: {
        encoding: string
        highWaterMark: number
        objectMode: boolean
        signal: AbortSignal
      }
    ): NodeJS.ReadableStream
  }
}

declare module 'node-fetch/src/body.js' {
  import type * as nodeBuffer from 'node:buffer'
  import type * as nodeFetch from 'node-fetch'

  // eslint-disable-next-line import/no-default-export
  export default class Body {
    constructor(
      body?: nodeFetch.BodyInit,
      options?: { size?: number }
    )

    readonly body: NodeJS.ReadableStream | null
    readonly bodyUsed: boolean
    readonly size: number

    arrayBuffer(): Promise<ArrayBuffer>
    formData(): Promise<FormData>
    blob(): Promise<nodeBuffer.Blob>
    json(): Promise<unknown>
    text(): Promise<string>
  }
}

interface ImportMeta {
  env: {
    [key: string]: string
  }
}

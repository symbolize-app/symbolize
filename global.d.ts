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

interface ParentNode {
  replaceChildren(
    ...nodesOrDOMStrings: (Node | string)[]
  ): void
}

interface ImportMeta {
  env: {
    [key: string]: string
  }

  resolve(
    specifier: string,
    parent?: string
  ): Promise<string>
}

declare module 'stream/promises' {
  import type { FinishedOptions } from 'stream'

  namespace promises {
    function finished(
      stream:
        | NodeJS.ReadableStream
        | NodeJS.WritableStream
        | NodeJS.ReadWriteStream,
      options?: FinishedOptions
    ): Promise<void>

    function pipeline(
      stream1: NodeJS.ReadableStream,
      stream2: NodeJS.WritableStream
    ): Promise<void>
    function pipeline(
      stream1: NodeJS.ReadableStream,
      stream2: NodeJS.ReadWriteStream,
      stream3: NodeJS.WritableStream
    ): Promise<void>
    function pipeline(
      stream1: NodeJS.ReadableStream,
      stream2: NodeJS.ReadWriteStream,
      stream3: NodeJS.ReadWriteStream,
      stream4: NodeJS.WritableStream
    ): Promise<void>
    function pipeline(
      stream1: NodeJS.ReadableStream,
      stream2: NodeJS.ReadWriteStream,
      stream3: NodeJS.ReadWriteStream,
      stream4: NodeJS.ReadWriteStream,
      stream5: NodeJS.WritableStream
    ): Promise<void>
    function pipeline(
      streams: ReadonlyArray<
        | NodeJS.ReadableStream
        | NodeJS.WritableStream
        | NodeJS.ReadWriteStream
      >
    ): Promise<void>
    function pipeline(
      stream1: NodeJS.ReadableStream,
      stream2:
        | NodeJS.ReadWriteStream
        | NodeJS.WritableStream,
      ...streams: Array<
        NodeJS.ReadWriteStream | NodeJS.WritableStream
      >
    ): Promise<void>
  }

  export = promises
}

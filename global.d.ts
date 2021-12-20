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

interface ImportMeta {
  env: {
    [key: string]: string
  }
}

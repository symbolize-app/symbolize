/* eslint-disable functional/prefer-immutable-types */
/* eslint-disable @typescript-eslint/naming-convention */

declare module 'fast-mersenne-twister' {
  export function MersenneTwister(seed: number[] | number): {
    genrand_int31(): number
    genrand_int32(): number
    genrand_real1(): number
    genrand_real2(): number
    genrand_real3(): number
    genrand_res53(): number
    random(): number
    random31Bit(): number
    random53Bit(): number
    randomExclusive(): number
    randomInclusive(): number
    randomNumber(): number
  }
}

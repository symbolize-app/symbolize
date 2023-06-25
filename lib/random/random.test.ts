import * as tinyRandom from '@intertwine/random/random.ts'
import * as tinyTest from '@intertwine/test/index.ts'
import * as fastMersenneTwister from 'fast-mersenne-twister'

export function initContext(): tinyRandom.Context {
  const twister =
    fastMersenneTwister.MersenneTwister(1616952581493)
  return {
    randomNumber: () => twister.random(),
    randomCryptoBits(bits) {
      const result = new Uint8Array(bits / 8)
      for (let i = 0; i < bits / 8; i += 4) {
        const fourBytes = twister.randomNumber()
        for (let j = 0; j < 4 && i + j < bits / 8; j++) {
          result[i + j] = fourBytes >> (j * 8)
        }
      }
      return result
    },
  }
}

export const url = import.meta.url

export const tests = {
  ['requestId']: (ctx: tinyTest.Context): void => {
    tinyTest.assertDeepEquals(
      tinyRandom.requestId(ctx),
      new Uint8Array([
        148, 25, 67, 83, 236, 194, 161, 68, 133, 3, 225, 39,
        117, 184, 162, 13, 201, 86, 169, 202, 38, 239, 16,
        242, 250, 147, 11, 231, 147, 27, 250, 116,
      ])
    )
  },
  ['requestIdHex']: (ctx: tinyTest.Context): void => {
    tinyTest.assertEquals(
      tinyRandom.requestIdHex(ctx),
      '94194353ecc2a1448503e12775b8a20dc956a9ca26ef10f2fa930be7931bfa74'
    )
  },
}

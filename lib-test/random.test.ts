import * as tinyRandom from '@intertwine/lib-random'

import * as tinyTest from '@/index.ts'

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

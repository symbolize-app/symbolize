import * as random from '@/index.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  ['requestId'](ctx: random.Context): void {
    test.assertDeepEquals(
      random.requestId(ctx),
      new Uint8Array([
        148, 25, 67, 83, 236, 194, 161, 68, 133, 3, 225, 39, 117, 184, 162,
        13, 201, 86, 169, 202, 38, 239, 16, 242, 250, 147, 11, 231, 147,
        27, 250, 116,
      ]),
    )
  },

  ['requestIdHex'](ctx: random.Context): void {
    test.assertEquals(
      random.requestIdHex(ctx),
      '94194353ecc2a1448503e12775b8a20dc956a9ca26ef10f2fa930be7931bfa74',
    )
  },
}

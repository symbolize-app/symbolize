import * as test from '@/index.ts'
import * as hex from '@intertwine/lib-hex'

export const url = import.meta.url

export const tests = {
  ['uint8ArrayToHex'](): void {
    test.assertDeepEquals(
      hex.uint8ArrayToHex(new Uint8Array([0x00, 0x23, 0xa0, 0xff])),
      '0023a0ff'
    )
  },

  ['uint8ArrayFromHex'](): void {
    test.assertDeepEquals(
      hex.uint8ArrayFromHex('0023a0ff'),
      new Uint8Array([0x00, 0x23, 0xa0, 0xff])
    )
  },
}

import * as tinyHex from '@intertwine/lib-hex/hex.ts'

import * as tinyTest from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['uint8ArrayToHex']: (): void => {
    tinyTest.assertDeepEquals(
      tinyHex.uint8ArrayToHex(
        new Uint8Array([0x00, 0x23, 0xa0, 0xff])
      ),
      '0023a0ff'
    )
  },
  ['uint8ArrayFromHex']: (): void => {
    tinyTest.assertDeepEquals(
      tinyHex.uint8ArrayFromHex('0023a0ff'),
      new Uint8Array([0x00, 0x23, 0xa0, 0xff])
    )
  },
}

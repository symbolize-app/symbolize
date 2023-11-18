import * as tinyHex from '@intertwine/lib-hex'

export type Context = {
  randomNumber(): number
  randomCryptoBits(bits: number): Uint8Array
}

export function initContext(): Context {
  return {
    randomNumber() {
      return Math.random()
    },
    randomCryptoBits(bits) {
      const result = new Uint8Array(bits / 8)
      crypto.getRandomValues(result)
      return result
    },
  }
}

export function requestId(ctx: Context): Uint8Array {
  return ctx.randomCryptoBits(256)
}

export function requestIdHex(ctx: Context): string {
  return tinyHex.uint8ArrayToHex(requestId(ctx))
}

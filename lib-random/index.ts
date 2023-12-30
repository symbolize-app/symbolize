import * as hex from '@intertwine/lib-hex'

export type Context = {
  random: {
    number(): number
    cryptoBits(bits: number): Uint8Array
  }
}

export function initContext(): Context {
  return {
    random: {
      number() {
        return Math.random()
      },
      cryptoBits(bits) {
        const result = new Uint8Array(bits / 8)
        crypto.getRandomValues(result)
        return result
      },
    },
  }
}

export function requestId(ctx: Context): Uint8Array {
  return ctx.random.cryptoBits(256)
}

export function requestIdHex(ctx: Context): string {
  return hex.uint8ArrayToHex(requestId(ctx))
}

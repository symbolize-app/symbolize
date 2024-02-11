import * as hex from '@intertwine/lib-hex'

export interface Context {
  readonly random: {
    cryptoBits(bits: number): Uint8Array
    number(): number
  }
}

export function initContext(): Context {
  return {
    random: {
      cryptoBits(bits) {
        const result = new Uint8Array(bits / 8)
        crypto.getRandomValues(result)
        return result
      },
      number() {
        return Math.random()
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

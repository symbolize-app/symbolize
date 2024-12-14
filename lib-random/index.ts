import * as hex from '@symbolize/lib-hex'

export interface Context {
  readonly random: Random
}

export interface Random {
  cryptoBits(bits: number): Uint8Array
  number(): number
}

class RandomImpl implements Random {
  cryptoBits(bits: number): Uint8Array {
    const result = new Uint8Array(bits / 8)
    crypto.getRandomValues(result)
    return result
  }

  number(): number {
    return Math.random()
  }
}

export function random(): Random {
  return new RandomImpl()
}

export function requestId(ctx: Context): Uint8Array {
  return ctx.random.cryptoBits(256)
}

export function requestIdHex(ctx: Context): string {
  return hex.uint8ArrayToHex(requestId(ctx))
}

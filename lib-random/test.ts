import type * as random_ from '@/index.ts'
import * as fastMersenneTwister from 'fast-mersenne-twister'

class RandomImpl implements random_.Random {
  constructor(
    private readonly twister: Readonly<
      ReturnType<typeof fastMersenneTwister.MersenneTwister>
    >,
  ) {}

  cryptoBits(bits: number): Uint8Array {
    const mutableResult = new Uint8Array(bits / 8)
    for (let i = 0; i < bits / 8; i += 4) {
      const fourBytes = this.twister.randomNumber()
      for (let j = 0; j < 4 && i + j < bits / 8; j++) {
        mutableResult[i + j] = fourBytes >> (j * 8)
      }
    }
    return mutableResult
  }

  number(): number {
    return this.twister.random()
  }
}

export function random(): random_.Random {
  const twister = fastMersenneTwister.MersenneTwister(1616952581493)
  return new RandomImpl(twister)
}

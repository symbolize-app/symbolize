import type * as random from '@intertwine/lib-random'
import * as fastMersenneTwister from 'fast-mersenne-twister'

export function initContext(): random.Context {
  const twister = fastMersenneTwister.MersenneTwister(1616952581493)
  return {
    random: {
      cryptoBits(bits) {
        const mutableResult = new Uint8Array(bits / 8)
        for (let i = 0; i < bits / 8; i += 4) {
          const fourBytes = twister.randomNumber()
          for (let j = 0; j < 4 && i + j < bits / 8; j++) {
            mutableResult[i + j] = fourBytes >> (j * 8)
          }
        }
        return mutableResult
      },
      number() {
        return twister.random()
      },
    },
  }
}

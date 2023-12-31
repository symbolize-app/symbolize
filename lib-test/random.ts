import type * as random from '@intertwine/lib-random'
import * as fastMersenneTwister from 'fast-mersenne-twister'

export function initContext(): random.Context {
  const twister = fastMersenneTwister.MersenneTwister(1616952581493)
  return {
    random: {
      number: () => twister.random(),
      cryptoBits(bits) {
        const result = new Uint8Array(bits / 8)
        for (let i = 0; i < bits / 8; i += 4) {
          const fourBytes = twister.randomNumber()
          for (let j = 0; j < 4 && i + j < bits / 8; j++) {
            result[i + j] = fourBytes >> (j * 8)
          }
        }
        return result
      },
    },
  }
}

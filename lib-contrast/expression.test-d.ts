import * as contrast from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['base'](): void {
    contrast.background.color(contrast.rgb(255, 0, 0))
  },

  ['null'](): void {
    contrast.background.color(null)
  },

  ['non-expression marker'](): void {
    const color = undefined as unknown as contrast.Color

    // @ts-expect-error -- can't put use marker as non-expression
    contrast.background.color(color)
  },

  ['multi'](): void {
    contrast.background.color([
      contrast.rgb(255, 0, 0),
      contrast.rgb(0, 255, 0),
    ])
    contrast.background.color(contrast.rgb([255, 0], 0, 0))
    contrast.background.color(contrast.rgb([255, [0, 1]], 0, 0))
  },

  ['pseudo'](): void {
    contrast.background.color(contrast.rgb(contrast.hover(255), 0, 0))
    contrast.background.color(contrast.rgb(0, contrast.hover(255), 0))
  },

  ['pseudo in multi'](): void {
    contrast.background.color([
      contrast.rgb(255, 0, 0),
      contrast.hover(contrast.rgb(0, 255, 0)),
    ])
  },

  ['multi in pseudo'](): void {
    contrast.background.color(
      contrast.rgb(contrast.hover([255, 254]), 0, 0),
    )
  },
}

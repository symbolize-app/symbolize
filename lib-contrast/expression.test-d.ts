import * as contrast from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['base'](): void {
    contrast.background.color(
      contrast.rgb(
        contrast.pct(100),
        contrast.pct(0),
        contrast.pct(0),
        contrast.pct(100),
      ),
    )
  },

  ['null'](): void {
    contrast.background.color(
      // @ts-expect-error -- unexpected null
      null,
    )
  },

  ['partial null'](): void {
    contrast.background.color(
      contrast.rgb(
        // @ts-expect-error -- unexpected null
        contrast.pct(100),
        null,
        contrast.pct(0),
        contrast.pct(0),
      ),
    )
  },

  ['multi'](): void {
    contrast.background.color(
      contrast.c(
        contrast.rgb(contrast.pct(100), contrast.pct(0), contrast.pct(0)),
        contrast.rgb(contrast.pct(0), contrast.pct(100), contrast.pct(0)),
      ),
    )
    contrast.background.color(
      contrast.rgb(
        contrast.c(contrast.pct(100), contrast.pct(0)),
        contrast.pct(0),
        contrast.pct(0),
      ),
    )
    contrast.background.color(
      contrast.rgb(
        contrast.c(
          contrast.pct(100),
          contrast.c(contrast.pct(0), contrast.pct(1)),
        ),
        contrast.pct(0),
        contrast.pct(0),
      ),
    )
  },

  ['pseudo'](): void {
    contrast.background.color(
      contrast.rgb(
        contrast.select.match(contrast.select.hover(), contrast.pct(100)),
        contrast.pct(0),
        contrast.pct(0),
        contrast.pct(100),
      ),
    )
    contrast.background.color(
      contrast.rgb(
        contrast.pct(0),
        contrast.select.match(contrast.select.hover(), contrast.pct(100)),
        contrast.pct(0),
        contrast.pct(100),
      ),
    )
  },

  ['pseudo in multi'](): void {
    contrast.background.color(
      contrast.c(
        contrast.rgb(contrast.pct(100), contrast.pct(0), contrast.pct(0)),
        contrast.select.match(
          contrast.select.hover(),
          contrast.rgb(
            contrast.pct(0),
            contrast.pct(100),
            contrast.pct(0),
          ),
        ),
      ),
    )
  },

  ['multi in pseudo'](): void {
    contrast.background.color(
      contrast.rgb(
        contrast.select.match(
          contrast.select.hover(),
          contrast.c(contrast.pct(100), contrast.pct(99)),
        ),
        contrast.pct(0),
        contrast.pct(0),
        contrast.pct(100),
      ),
    )
  },
}

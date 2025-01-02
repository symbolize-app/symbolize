import * as styling from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['base'](): void {
    styling.background.color(
      styling.rgb(
        styling.pct(100),
        styling.pct(0),
        styling.pct(0),
        styling.pct(100),
      ),
    )
  },

  ['null'](): void {
    styling.background.color(
      // @ts-expect-error -- unexpected null
      null,
    )
  },

  ['partial null'](): void {
    styling.background.color(
      styling.rgb(
        // @ts-expect-error -- unexpected null
        styling.pct(100),
        null,
        styling.pct(0),
        styling.pct(0),
      ),
    )
  },

  ['multi'](): void {
    styling.background.color(
      styling.c(
        styling.rgb(styling.pct(100), styling.pct(0), styling.pct(0)),
        styling.rgb(styling.pct(0), styling.pct(100), styling.pct(0)),
      ),
    )
    styling.background.color(
      styling.rgb(
        styling.c(styling.pct(100), styling.pct(0)),
        styling.pct(0),
        styling.pct(0),
      ),
    )
    styling.background.color(
      styling.rgb(
        styling.c(
          styling.pct(100),
          styling.c(styling.pct(0), styling.pct(1)),
        ),
        styling.pct(0),
        styling.pct(0),
      ),
    )
  },

  ['pseudo'](): void {
    styling.background.color(
      styling.rgb(
        styling.select.match(styling.select.hover(), styling.pct(100)),
        styling.pct(0),
        styling.pct(0),
        styling.pct(100),
      ),
    )
    styling.background.color(
      styling.rgb(
        styling.pct(0),
        styling.select.match(styling.select.hover(), styling.pct(100)),
        styling.pct(0),
        styling.pct(100),
      ),
    )
  },

  ['pseudo in multi'](): void {
    styling.background.color(
      styling.c(
        styling.rgb(styling.pct(100), styling.pct(0), styling.pct(0)),
        styling.select.match(
          styling.select.hover(),
          styling.rgb(styling.pct(0), styling.pct(100), styling.pct(0)),
        ),
      ),
    )
  },

  ['multi in pseudo'](): void {
    styling.background.color(
      styling.rgb(
        styling.select.match(
          styling.select.hover(),
          styling.c(styling.pct(100), styling.pct(99)),
        ),
        styling.pct(0),
        styling.pct(0),
        styling.pct(100),
      ),
    )
  },
}

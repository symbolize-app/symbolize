import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['add / percent'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<styling.Pct>()
    const style = [
      a.set(styling.add(styling.pct(1), styling.pct(2), styling.pct(3))),
    ]
    const code = '--s0: calc(1% + 2% + 3%)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['add / number'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<number>()
    const style = [a.set(styling.add(1, 2, 3))]
    const code = '--s0: calc(1 + 2 + 3)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['clamp / percent'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<styling.Pct>()
    const style = [
      a.set(styling.clamp(styling.pct(1), styling.pct(2), styling.pct(3))),
    ]
    const code = '--s0: clamp(1%, 2%, 3%)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['clamp / number'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<number>()
    const style = [a.set(styling.clamp(1, 2, 3))]
    const code = '--s0: clamp(1, 2, 3)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['div / percent'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<styling.Pct>()
    const style = [a.set(styling.div(styling.pct(1), 2, 3))]
    const code = '--s0: calc(1% / 2 / 3)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['div / number'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<number>()
    const style = [a.set(styling.div(1, 2, 3))]
    const code = '--s0: calc(1 / 2 / 3)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['min / percent'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<styling.Pct>()
    const style = [
      a.set(styling.min(styling.pct(1), styling.pct(2), styling.pct(3))),
    ]
    const code = '--s0: min(1%, 2%, 3%)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['min / number'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<number>()
    const style = [a.set(styling.min(1, 2, 3))]
    const code = '--s0: min(1, 2, 3)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['max / percent'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<styling.Pct>()
    const style = [
      a.set(styling.max(styling.pct(1), styling.pct(2), styling.pct(3))),
    ]
    const code = '--s0: max(1%, 2%, 3%)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['max / number'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<number>()
    const style = [a.set(styling.max(1, 2, 3))]
    const code = '--s0: max(1, 2, 3)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['mul / percent'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<styling.Pct>()
    const style = [a.set(styling.mul(styling.pct(1), 2, 3))]
    const code = '--s0: calc(1% * 2 * 3)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['mul / number'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<number>()
    const style = [a.set(styling.mul(1, 2, 3))]
    const code = '--s0: calc(1 * 2 * 3)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['sub / percent'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<styling.Pct>()
    const style = [
      a.set(styling.sub(styling.pct(1), styling.pct(2), styling.pct(3))),
    ]
    const code = '--s0: calc(1% - 2% - 3%)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}

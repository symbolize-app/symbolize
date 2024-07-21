import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['add / percent'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<contrast.Pct>()
    const style = [
      a.set(
        contrast.add(contrast.pct(1), contrast.pct(2), contrast.pct(3)),
      ),
    ]
    const code = '--s0: calc(1% + 2% + 3%)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['add / number'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<number>()
    const style = [a.set(contrast.add(1, 2, 3))]
    const code = '--s0: calc(1 + 2 + 3)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['clamp / percent'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<contrast.Pct>()
    const style = [
      a.set(
        contrast.clamp(contrast.pct(1), contrast.pct(2), contrast.pct(3)),
      ),
    ]
    const code = '--s0: clamp(1%, 2%, 3%)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['clamp / number'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<number>()
    const style = [a.set(contrast.clamp(1, 2, 3))]
    const code = '--s0: clamp(1, 2, 3)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['div / percent'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<contrast.Pct>()
    const style = [a.set(contrast.div(contrast.pct(1), 2, 3))]
    const code = '--s0: calc(1% / 2 / 3)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['div / number'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<number>()
    const style = [a.set(contrast.div(1, 2, 3))]
    const code = '--s0: calc(1 / 2 / 3)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['min / percent'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<contrast.Pct>()
    const style = [
      a.set(
        contrast.min(contrast.pct(1), contrast.pct(2), contrast.pct(3)),
      ),
    ]
    const code = '--s0: min(1%, 2%, 3%)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['min / number'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<number>()
    const style = [a.set(contrast.min(1, 2, 3))]
    const code = '--s0: min(1, 2, 3)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['max / percent'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<contrast.Pct>()
    const style = [
      a.set(
        contrast.max(contrast.pct(1), contrast.pct(2), contrast.pct(3)),
      ),
    ]
    const code = '--s0: max(1%, 2%, 3%)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['max / number'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<number>()
    const style = [a.set(contrast.max(1, 2, 3))]
    const code = '--s0: max(1, 2, 3)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['mul / percent'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<contrast.Pct>()
    const style = [a.set(contrast.mul(contrast.pct(1), 2, 3))]
    const code = '--s0: calc(1% * 2 * 3)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['mul / number'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<number>()
    const style = [a.set(contrast.mul(1, 2, 3))]
    const code = '--s0: calc(1 * 2 * 3)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['sub / percent'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<contrast.Pct>()
    const style = [
      a.set(
        contrast.sub(contrast.pct(1), contrast.pct(2), contrast.pct(3)),
      ),
    ]
    const code = '--s0: calc(1% - 2% - 3%)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}

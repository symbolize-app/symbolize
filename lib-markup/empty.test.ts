import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['empty'](
    ctx: compute.Context & contrast.Context & markup.Context,
  ): Promise<void> {
    const fragment = markup.empty()
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)
  },

  async ['empty opt'](
    ctx: compute.Context & contrast.Context & markup.Context,
  ): Promise<void> {
    const fragment = markup.toFragment(null)
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)
  },
}

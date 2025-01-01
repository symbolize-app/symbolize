import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import type * as dataflow from '@symbolize/lib-dataflow'
import type * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['empty'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.empty()
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)
  },

  async ['empty opt'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.toFragment(null)
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)
  },
}

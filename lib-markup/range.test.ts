import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['range'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.range({
      content: [
        markup.text({ content: 'a' }),
        markup.text({ content: 'b' }),
      ],
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, 'ab')
  },

  async ['range opt'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.toFragment(['a', 'b'])
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, 'ab')
  },
}

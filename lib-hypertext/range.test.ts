import * as hypertext from '@/index.ts'
import * as hypertextTest from '@/test.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['range'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const fragment = hypertext.range({
      content: [
        hypertext.text({ content: 'a' }),
        hypertext.text({ content: 'b' }),
      ],
    })
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, 'ab')
  },

  async ['range opt'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const fragment = hypertext.toFragment(['a', 'b'])
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, 'ab')
  },
}

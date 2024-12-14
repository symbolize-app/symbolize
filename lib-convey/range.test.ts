import * as convey from '@/index.ts'
import * as conveyTest from '@/test.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['range'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.range({
      content: [
        convey.text({ content: 'a' }),
        convey.text({ content: 'b' }),
      ],
    })
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, 'ab')
  },

  async ['range opt'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.toFragment(['a', 'b'])
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, 'ab')
  },
}

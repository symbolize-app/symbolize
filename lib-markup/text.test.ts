import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import * as compute from '@symbolize/lib-compute'
import type * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['text pure'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.text({ content: 'hello' })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text opt'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.toFragment('hello')
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text state'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const x = compute.state('a')

    const fragment = markup.text({ content: x })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'a')

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'b')
    })
    test.assertEquals(body.textContent, 'b')

    await fragment.remove()
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'c')
    })
    test.assertEquals(body.textContent, 'b')
  },
}

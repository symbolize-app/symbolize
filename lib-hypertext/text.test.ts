import * as hypertext from '@/index.ts'
import * as hypertextTest from '@/test.ts'
import * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['text pure'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const fragment = hypertext.text({ content: 'hello' })
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text opt'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const fragment = hypertext.toFragment('hello')
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text state'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const x = compute.state('a')

    const fragment = hypertext.text({ content: x })
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
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

import * as convey from '@/index.ts'
import * as conveyTest from '@/test.ts'
import * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['text pure'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.text({ content: 'hello' })
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text opt'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.toFragment('hello')
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text state'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const x = compute.state('a')

    const fragment = convey.text({ content: x })
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
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

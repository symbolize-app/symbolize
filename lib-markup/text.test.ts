import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import * as dataflow from '@symbolize/lib-dataflow'
import type * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['text pure'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.text({ content: 'hello' })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text opt'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.toFragment('hello')
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text state'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const x = dataflow.state('a')

    const fragment = markup.text({ content: x })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'a')

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, x, 'b')
    })
    test.assertEquals(body.textContent, 'b')

    await fragment.remove()
    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, x, 'c')
    })
    test.assertEquals(body.textContent, 'b')
  },
}

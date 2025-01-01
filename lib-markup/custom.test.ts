/* eslint-disable @typescript-eslint/no-empty-function */
import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import * as compute from '@symbolize/lib-compute'
import type * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['custom empty'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const custom = markup.defineCustom((_ctx, _attrs) => {
      return null
    })
    const fragment = custom({})
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)
  },

  async ['custom pure'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const custom = markup.defineCustom<
      unknown,
      {
        readonly title: compute.NodeOpt<string>
      }
    >((_ctx, attrs) => {
      return markup.text({
        content: compute.map((title) => `${title} / 0`, attrs.title),
      })
    })

    const fragment = custom({ title: 'hello' })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'hello / 0')
  },

  async ['custom effect'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const [effectCallback, effectCallbackHistory] =
      test.repeatMockWithHistory(2, (_value: string) => {})

    const custom = markup.defineCustom<
      unknown,
      {
        readonly title: compute.NodeOpt<string>
      }
    >(async (ctx, attrs) => {
      await markup.scopedEffect(ctx, effectCallback, attrs.title)
      return attrs.title
    })

    const x = compute.state('a')

    const fragment = custom({ title: x })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'a')
    test.assertDeepEquals(effectCallbackHistory, [['a']])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'b')
    })
    test.assertEquals(body.textContent, 'b')
    test.assertDeepEquals(effectCallbackHistory, [['a'], ['b']])

    await fragment.remove()
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'c')
    })
    test.assertEquals(body.textContent, 'b')
    test.assertDeepEquals(effectCallbackHistory, [['a'], ['b']])
  },

  async ['custom defer'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const [deferCallback, deferCallbackHistory] =
      test.repeatMockWithHistory(2, () => {})

    const custom = markup.defineCustom<
      unknown,
      {
        readonly title: compute.NodeOpt<string>
      }
    >((ctx, attrs) => {
      markup.scopedDefer(ctx, deferCallback)
      return attrs.title
    })

    const fragment = custom({ title: 'x' })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'x')
    test.assertDeepEquals(deferCallbackHistory, [])

    await fragment.remove()
    test.assertDeepEquals(deferCallbackHistory, [[]])
  },
}

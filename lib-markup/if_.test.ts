/* eslint-disable @typescript-eslint/no-empty-function */
import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import * as dataflow from '@symbolize/lib-dataflow'
import type * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['if basic'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const y = dataflow.state({ x: 2 } as { readonly x: number } | null)

    const [init, initHistory] = test.repeatMockWithHistory(
      2,
      (_x: number) => {},
    )
    const custom = markup.defineCustom<
      unknown,
      {
        readonly y: dataflow.Computation<{
          readonly x: number
        }>
      }
    >(async (_ctx, attrs) => {
      init(await dataflow.value(attrs.y.x))
      return dataflow.map((y) => `${y.x * 3}`, attrs.y)
    })
    const fragment = markup.if_(
      (y) => custom({ y }),
      () => 'nothing',
      y,
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, '6')
    test.assertDeepEquals(initHistory, [[2]])

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, y, { x: 4 })
    })
    test.assertEquals(body.textContent, '12')
    test.assertDeepEquals(initHistory, [[2]])

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, y, null)
    })
    test.assertEquals(body.textContent, 'nothing')
    test.assertDeepEquals(initHistory, [[2]])

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, y, { x: 3 })
    })
    test.assertEquals(body.textContent, '9')
    test.assertDeepEquals(initHistory, [[2], [3]])

    await fragment.remove()
    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, y, { x: 5 })
    })
    test.assertEquals(body.textContent, '9')
    test.assertDeepEquals(initHistory, [[2], [3]])
  },

  async ['if inner count'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const x = dataflow.state('a')

    const fragment = markup.if_(
      () => [
        markup.html.div({ content: '_' }),
        markup.html.div({ content: x }),
      ],
      () => null,
      dataflow.map((x) => x.startsWith('a'), x),
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.children.length, 2)
    test.assertEquals(body.childNodes.length, 4)
    test.assertEquals(body.textContent, '_a')

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, x, 'b')
    })
    test.assertEquals(body.children.length, 0)
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, '')

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, x, 'ab')
    })
    test.assertEquals(body.children.length, 2)
    test.assertEquals(body.childNodes.length, 4)
    test.assertEquals(body.textContent, '_ab')
  },

  async ['if lazy true'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.if_(
      () =>
        markup.defineCustom(() => {
          throw new Error()
        })({}),
      () => 'ok',
      false,
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'ok')
  },

  async ['if lazy false'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.if_(
      () =>
        markup.defineCustom(() => {
          throw new Error()
        })({}),
      () => 'ok',
      false,
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'ok')
  },

  async ['if nested'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const x = dataflow.state(true)
    const y = dataflow.state(true)

    const [init, initHistory] = test.repeatMockWithHistory(
      10,
      (_name: string) => {},
    )
    const custom = markup.defineCustom<
      unknown,
      {
        readonly name: dataflow.NodeOpt<string>
      }
    >(async (_ctx, attrs) => {
      init(await dataflow.value(attrs.name))
      return markup.html.div({ content: attrs.name })
    })
    const fragment = markup.if_(
      () =>
        markup.if_(
          () => [custom({ name: '0' })],
          () => [custom({ name: '1' }), custom({ name: '2' })],
          y,
        ),
      () =>
        markup.if_(
          () => [
            custom({ name: '3' }),
            custom({ name: '4' }),
            custom({ name: '5' }),
          ],
          () => [
            custom({ name: '6' }),
            custom({ name: '7' }),
            custom({ name: '8' }),
            custom({ name: '9' }),
          ],
          y,
        ),
      x,
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, '0')
    test.assertEquals(initHistory.map(([name]) => name).join(''), '0')

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, x, false)
    })
    test.assertEquals(body.textContent, '345')
    test.assertEquals(initHistory.map(([name]) => name).join(''), '0345')

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, y, false)
    })
    test.assertEquals(body.textContent, '6789')
    test.assertEquals(
      initHistory.map(([name]) => name).join(''),
      '03456789',
    )

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, x, true)
    })
    test.assertEquals(body.textContent, '12')
    test.assertEquals(
      initHistory.map(([name]) => name).join(''),
      '0345678912',
    )
  },
}

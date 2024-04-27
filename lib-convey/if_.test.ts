/* eslint-disable @typescript-eslint/no-empty-function */
import * as convey from '@/index.ts'
import * as compute from '@intertwine/lib-compute'
import * as test from '@intertwine/lib-test'
import arrayFromAsync from 'core-js-pure/actual/array/from-async'

export const url = import.meta.url

export const tests = {
  async ['if basic'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const y = compute.state({ x: 2 } as { readonly x: number } | null)

    const [init, initHistory] = test.repeatMockWithHistory(
      2,
      (_x: number) => {},
    )
    const custom = convey.defineCustom<
      unknown,
      {
        readonly y: compute.Computation<{
          readonly x: number
        }>
      }
    >(async (_ctx, attrs) => {
      init(await compute.value(attrs.y.x))
      return compute.map((y) => `${y.x * 3}`, attrs.y)
    })
    const fragment = convey.if_(
      (y) => custom({ y }),
      () => 'nothing',
      y,
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, '6')
    test.assertDeepEquals(initHistory, [[2]])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, y, { x: 4 })
    })
    test.assertEquals(body.textContent, '12')
    test.assertDeepEquals(initHistory, [[2]])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, y, null)
    })
    test.assertEquals(body.textContent, 'nothing')
    test.assertDeepEquals(initHistory, [[2]])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, y, { x: 3 })
    })
    test.assertEquals(body.textContent, '9')
    test.assertDeepEquals(initHistory, [[2], [3]])

    await fragment.remove()
    await compute.txn(ctx, async () => {
      await compute.set(ctx, y, { x: 5 })
    })
    test.assertEquals(body.textContent, '9')
    test.assertDeepEquals(initHistory, [[2], [3]])
  },

  async ['if inner count'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const x = compute.state('a')

    const fragment = convey.if_(
      () => [
        convey.html.div({ content: '_' }),
        convey.html.div({ content: x }),
      ],
      () => null,
      compute.map((x) => x.startsWith('a'), x),
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.children.length, 2)
    test.assertEquals(body.childNodes.length, 4)
    test.assertEquals(body.textContent, '_a')

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'b')
    })
    test.assertEquals(body.children.length, 0)
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, '')

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'ab')
    })
    test.assertEquals(body.children.length, 2)
    test.assertEquals(body.childNodes.length, 4)
    test.assertEquals(body.textContent, '_ab')
  },

  async ['if lazy true'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.if_(
      () =>
        convey.defineCustom(() => {
          throw new Error()
        })({}),
      () => 'ok',
      false,
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, 'ok')
  },

  async ['if lazy false'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.if_(
      () =>
        convey.defineCustom(() => {
          throw new Error()
        })({}),
      () => 'ok',
      false,
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, 'ok')
  },

  async ['if nested'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const x = compute.state(true)
    const y = compute.state(true)

    const [init, initHistory] = test.repeatMockWithHistory(
      10,
      (_name: string) => {},
    )
    const custom = convey.defineCustom<
      unknown,
      {
        readonly name: compute.NodeOpt<string>
      }
    >(async (_ctx, attrs) => {
      init(await compute.value(attrs.name))
      return convey.html.div({ content: attrs.name })
    })
    const fragment = convey.if_(
      () =>
        convey.if_(
          () => [custom({ name: '0' })],
          () => [custom({ name: '1' }), custom({ name: '2' })],
          y,
        ),
      () =>
        convey.if_(
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
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, '0')
    test.assertEquals(initHistory.map(([name]) => name).join(''), '0')

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, false)
    })
    test.assertEquals(body.textContent, '345')
    test.assertEquals(initHistory.map(([name]) => name).join(''), '0345')

    await compute.txn(ctx, async () => {
      await compute.set(ctx, y, false)
    })
    test.assertEquals(body.textContent, '6789')
    test.assertEquals(
      initHistory.map(([name]) => name).join(''),
      '03456789',
    )

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, true)
    })
    test.assertEquals(body.textContent, '12')
    test.assertEquals(
      initHistory.map(([name]) => name).join(''),
      '0345678912',
    )
  },
}

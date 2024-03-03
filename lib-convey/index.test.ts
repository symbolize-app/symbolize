/* eslint-disable @typescript-eslint/no-empty-function */
import * as convey from '@/index.ts'
import * as compute from '@intertwine/lib-compute'
import * as test from '@intertwine/lib-test'
import arrayFromAsync from 'core-js-pure/actual/array/from-async'

export const url = import.meta.url

export const tests = {
  async ['empty'](ctx: compute.Context & convey.Context): Promise<void> {
    test.assertDeepEquals(
      await arrayFromAsync(convey.empty().add(ctx)),
      [],
    )
  },

  async ['empty opt'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    test.assertDeepEquals(
      await arrayFromAsync(convey.toFragment(null).add(ctx)),
      [],
    )
  },

  async ['text pure'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const body = ctx.convey.document.body
    body.append(
      ...(await arrayFromAsync(
        convey.text({ content: 'hello' }).add(ctx),
      )),
    )
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text opt'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const body = ctx.convey.document.body
    body.append(
      ...(await arrayFromAsync(convey.toFragment('hello').add(ctx))),
    )
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text state'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const x = compute.state('a')

    const fragment = convey.text({ content: x })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
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

  async ['range'](ctx: compute.Context & convey.Context): Promise<void> {
    const body = ctx.convey.document.body
    body.append(
      ...(await arrayFromAsync(
        convey
          .range({
            content: [
              convey.text({ content: 'a' }),
              convey.text({ content: 'b' }),
            ],
          })
          .add(ctx),
      )),
    )
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, 'ab')
  },

  async ['range opt'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const body = ctx.convey.document.body
    body.append(
      ...(await arrayFromAsync(convey.toFragment(['a', 'b']).add(ctx))),
    )
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, 'ab')
  },

  async ['custom empty'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const custom = convey.defineCustom((_ctx, _attrs) => {
      return null
    })
    test.assertDeepEquals(await arrayFromAsync(custom({}).add(ctx)), [])
  },

  async ['custom pure'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const custom = convey.defineCustom<
      unknown,
      {
        readonly title: compute.ComputationOpt<string>
      }
    >((_ctx, attrs) => {
      return convey.text({
        content: compute.map((title) => `${title} / 0`, attrs.title),
      })
    })

    const body = ctx.convey.document.body
    body.append(
      ...(await arrayFromAsync(custom({ title: 'hello' }).add(ctx))),
    )
    test.assertEquals(body.textContent, 'hello / 0')
  },

  async ['custom effect'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const [effectCallback, effectCallbackHistory] =
      test.repeatMockWithHistory(2, (_value: string) => {})

    const custom = convey.defineCustom<
      unknown,
      {
        readonly title: compute.ComputationOpt<string>
      }
    >(async (ctx, attrs) => {
      await convey.scopedEffect(ctx, effectCallback, attrs.title)
      return attrs.title
    })

    const x = compute.state('a')

    const fragment = custom({ title: x })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
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
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const [deferCallback, deferCallbackHistory] =
      test.repeatMockWithHistory(2, () => {})

    const custom = convey.defineCustom<
      unknown,
      {
        readonly title: compute.ComputationOpt<string>
      }
    >((ctx, attrs) => {
      convey.scopedDefer(ctx, deferCallback)
      return attrs.title
    })

    const fragment = custom({ title: 'x' })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, 'x')
    test.assertDeepEquals(deferCallbackHistory, [])

    await fragment.remove()
    test.assertDeepEquals(deferCallbackHistory, [[]])
  },

  async ['div advanced'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const custom = convey.defineCustom<
      unknown,
      {
        readonly title: compute.ComputationOpt<string>
      }
    >((ctx, attrs) => {
      const countState = compute.state(0)

      return convey.div({
        onclick: compute.handler(async (_event, count) => {
          await compute.set(ctx, countState, count + 1)
        }, countState),

        content: compute.map(
          (title, count) => `${title} / ${count}`,
          attrs.title,
          countState,
        ),
      })
    })

    const fragment = custom({ title: 'hello' })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const div = body.querySelector('div')
    test.assert(div)
    test.assertEquals(div.textContent, 'hello / 0')

    div.click()
    await convey.wait(ctx)
    test.assertEquals(div.textContent, 'hello / 1')

    await fragment.remove()
    div.click()
    await convey.wait(ctx)
    test.assertEquals(div.textContent, 'hello / 1')
  },

  async ['div pure'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const [clickCallback, clickCallbackHistory] =
      test.repeatMockWithHistory(1, (_event: Readonly<MouseEvent>) => {})

    const fragment = convey.div({
      id: 'x',
      onclick: clickCallback,

      content: 'y',
    })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const div = body.querySelector('div')
    test.assert(div)
    test.assertEquals(div.id, 'x')
    test.assertEquals(div.textContent, 'y')
    test.assertDeepEquals(clickCallbackHistory, [])

    div.click()
    await convey.wait(ctx)
    test.assertEquals(clickCallbackHistory.length, 1)
  },

  async ['div state'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const x = compute.state('a')

    const fragment = convey.div({ id: x })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const div = body.querySelector('div')
    test.assert(div)
    test.assertEquals(div.id, 'a')

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'b')
    })
    test.assertEquals(div.id, 'b')

    await fragment.remove()
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'c')
    })
    test.assertEquals(div.id, 'b')
  },

  async ['match basic'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const x = compute.state({ x: 2 } as { readonly x: number } | null)

    const [init, initHistory] = test.repeatMockWithHistory(
      2,
      (_x: number) => {},
    )
    const custom = convey.defineCustom<
      unknown,
      {
        readonly x: { readonly x: number }
      }
    >(async (_ctx, attrs) => {
      init(await compute.value(attrs.x.x))
      return compute.map((x) => `${x.x * 3}`, attrs.x)
    })
    const fragment = convey.if_(
      (x) => custom({ x }),
      () => 'nothing',
      x,
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, '6')
    test.assertDeepEquals(initHistory, [[2]])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, { x: 4 })
    })
    test.assertEquals(body.textContent, '12')
    test.assertDeepEquals(initHistory, [[2]])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, null)
    })
    test.assertEquals(body.textContent, 'nothing')
    test.assertDeepEquals(initHistory, [[2]])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, { x: 3 })
    })
    test.assertEquals(body.textContent, '9')
    test.assertDeepEquals(initHistory, [[2], [3]])

    await fragment.remove()
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, { x: 5 })
    })
    test.assertEquals(body.textContent, '9')
    test.assertDeepEquals(initHistory, [[2], [3]])
  },

  async ['match inner count'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const x = compute.state('a')

    const fragment = convey.if_(
      () => [convey.div({ content: '_' }), convey.div({ content: x })],
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

  async ['match lazy true'](
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

  async ['match lazy false'](
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

  async ['match nested'](
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
        readonly name: compute.ComputationOpt<string>
      }
    >(async (_ctx, attrs) => {
      init(await compute.value(attrs.name))
      return convey.div({ content: attrs.name })
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

  async ['items pure'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.each(
      (x) => convey.div({ content: x }),
      (x) => x,
      ['a', 'b', 'c'],
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, 'abc')
  },

  async ['items inner state'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const items = compute.state([
      { id: 0, name: 'a' },
      { id: 1, name: 'b' },
      { id: 2, name: 'c' },
    ])

    const [init, initHistory] = test.repeatMockWithHistory(
      3,
      (_name: string) => {},
    )
    const custom = convey.defineCustom<
      unknown,
      {
        readonly name: compute.ComputationOpt<string>
      }
    >(async (_ctx, attrs) => {
      init(await compute.value(attrs.name))
      return convey.div({ content: attrs.name })
    })
    const fragment = convey.each(
      (item) => custom({ name: item.name }),
      (item) => item.id,
      items,
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, 'abc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, items, [
        await compute.value(items[0]),
        await compute.value(items[1]),
        { id: 2, name: 'd' },
      ])
    })
    test.assertEquals(body.textContent, 'abd')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])
  },

  async ['items move'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const items = compute.state([
      { id: 0, name: 'a' },
      { id: 1, name: 'b' },
      { id: 2, name: 'c' },
    ])

    const [init, initHistory] = test.repeatMockWithHistory(
      3,
      (_name: string) => {},
    )
    const custom = convey.defineCustom<
      unknown,
      {
        readonly name: compute.ComputationOpt<string>
      }
    >(async (_ctx, attrs) => {
      init(await compute.value(attrs.name))
      return convey.div({ content: attrs.name })
    })
    const fragment = convey.each(
      (item) => custom({ name: item.name }),
      (item) => item.id,
      items,
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, 'abc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, items, [
        await compute.value(items[2]),
        await compute.value(items[0]),
        await compute.value(items[1]),
      ])
    })
    test.assertEquals(body.textContent, 'cab')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])
  },

  async ['items remove'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const items = compute.state([
      { id: 0, name: 'a' },
      { id: 1, name: 'b' },
      { id: 2, name: 'c' },
    ])

    const [init, initHistory] = test.repeatMockWithHistory(
      3,
      (_name: string) => {},
    )
    const custom = convey.defineCustom<
      unknown,
      {
        readonly name: compute.ComputationOpt<string>
      }
    >(async (_ctx, attrs) => {
      init(await compute.value(attrs.name))
      return convey.div({ content: attrs.name })
    })
    const fragment = convey.each(
      (item) => custom({ name: item.name }),
      (item) => item.id,
      items,
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, 'abc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, items, [
        await compute.value(items[0]),
        await compute.value(items[2]),
      ])
    })
    test.assertEquals(body.textContent, 'ac')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])
  },

  async ['items add'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const items = compute.state([
      { id: 0, name: 'a' },
      { id: 1, name: 'b' },
      { id: 2, name: 'c' },
    ])

    const [init, initHistory] = test.repeatMockWithHistory(
      4,
      (_name: string) => {},
    )
    const custom = convey.defineCustom<
      unknown,
      {
        readonly name: compute.ComputationOpt<string>
      }
    >(async (_ctx, attrs) => {
      init(await compute.value(attrs.name))
      return convey.div({ content: attrs.name })
    })
    const fragment = convey.each(
      (item) => custom({ name: item.name }),
      (item) => item.id,
      items,
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    test.assertEquals(body.textContent, 'abc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, items, [
        await compute.value(items[0]),
        await compute.value(items[1]),
        { id: 3, name: 'd' },
        await compute.value(items[2]),
      ])
    })
    test.assertEquals(body.textContent, 'abdc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c'], ['d']])

    await fragment.remove()
    await compute.txn(ctx, async () => {
      await compute.set(ctx, items, [
        await compute.value(items[0]),
        await compute.value(items[1]),
        { id: 4, name: 'e' },
        await compute.value(items[2]),
      ])
    })
    test.assertEquals(body.textContent, 'abdc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c'], ['d']])
  },
}

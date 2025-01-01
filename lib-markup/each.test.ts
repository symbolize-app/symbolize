/* eslint-disable @typescript-eslint/no-empty-function */
import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import * as dataflow from '@symbolize/lib-dataflow'
import type * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['items pure'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.each(
      (x) => markup.html.div({ content: x }),
      (x) => x,
      ['a', 'b', 'c'],
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'abc')
  },

  async ['items inner state'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const items = dataflow.state([
      { id: 0, name: 'a' },
      { id: 1, name: 'b' },
      { id: 2, name: 'c' },
    ])

    const [init, initHistory] = test.repeatMockWithHistory(
      3,
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
    const fragment = markup.each(
      (item) => custom({ name: item.name }),
      (item) => item.id,
      items,
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'abc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, items, [
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[0]))!,
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[1]))!,
        { id: 2, name: 'd' },
      ])
    })
    test.assertEquals(body.textContent, 'abd')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])
  },

  async ['items move'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const items = dataflow.state([
      { id: 0, name: 'a' },
      { id: 1, name: 'b' },
      { id: 2, name: 'c' },
    ])

    const [init, initHistory] = test.repeatMockWithHistory(
      3,
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
    const fragment = markup.each(
      (item) => custom({ name: item.name }),
      (item) => item.id,
      items,
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'abc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, items, [
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[2]))!,
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[0]))!,
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[1]))!,
      ])
    })
    test.assertEquals(body.textContent, 'cab')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])
  },

  async ['items remove'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const items = dataflow.state([
      { id: 0, name: 'a' },
      { id: 1, name: 'b' },
      { id: 2, name: 'c' },
    ])

    const [init, initHistory] = test.repeatMockWithHistory(
      3,
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
    const fragment = markup.each(
      (item) => custom({ name: item.name }),
      (item) => item.id,
      items,
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'abc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, items, [
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[0]))!,
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[2]))!,
      ])
    })
    test.assertEquals(body.textContent, 'ac')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])
  },

  async ['items add'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const items = dataflow.state([
      { id: 0, name: 'a' },
      { id: 1, name: 'b' },
      { id: 2, name: 'c' },
    ])

    const [init, initHistory] = test.repeatMockWithHistory(
      4,
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
    const fragment = markup.each(
      (item) => custom({ name: item.name }),
      (item) => item.id,
      items,
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.textContent, 'abc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c']])

    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, items, [
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[0]))!,
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[1]))!,
        { id: 3, name: 'd' },
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[2]))!,
      ])
    })
    test.assertEquals(body.textContent, 'abdc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c'], ['d']])

    await fragment.remove()
    await dataflow.txn(ctx, async () => {
      await dataflow.set(ctx, items, [
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[0]))!,
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[1]))!,
        { id: 4, name: 'e' },
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- simple test
        (await dataflow.value(items[2]))!,
      ])
    })
    test.assertEquals(body.textContent, 'abdc')
    test.assertDeepEquals(initHistory, [['a'], ['b'], ['c'], ['d']])
  },
}

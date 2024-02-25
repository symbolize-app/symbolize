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
    await new Promise((resolve) => setTimeout(resolve, 0))
    test.assertEquals(div.textContent, 'hello / 1')

    await fragment.remove()
    div.click()
    await new Promise((resolve) => setTimeout(resolve, 0))
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
    await new Promise((resolve) => setTimeout(resolve, 0))
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
}

/* eslint-disable @typescript-eslint/no-empty-function */
import * as convey from '@/index.ts'
import * as compute from '@intertwine/lib-compute'
import * as test from '@intertwine/lib-test'
import arrayFromAsync from 'core-js-pure/actual/array/from-async'

export const url = import.meta.url

export const tests = {
  async ['div advanced'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const custom = convey.defineCustom<
      unknown,
      {
        readonly title: compute.NodeOpt<string>
      }
    >((ctx, attrs) => {
      const countState = compute.state(0)

      return convey.html.div({
        onClick: compute.handler(async (_event, count) => {
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

    const fragment = convey.html.div({
      ariaDisabled: true,
      id: 'x',
      onClick: clickCallback,

      content: 'y',
    })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const div = body.querySelector('div')
    test.assert(div)
    test.assertEquals(div.id, 'x')
    test.assertEquals(div.textContent, 'y')
    test.assertEquals(
      div.outerHTML,
      '<div aria-disabled="true" id="x">y</div>',
    )
    test.assertDeepEquals(clickCallbackHistory, [])

    div.click()
    await convey.wait(ctx)
    test.assertEquals(clickCallbackHistory.length, 1)
  },

  async ['div state'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const x = compute.state('a')

    const fragment = convey.html.div({ id: x })
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

  async ['div string null'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const title = compute.state<string | null>('a')

    const fragment = convey.html.div({ title })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const div = body.querySelector('div')
    test.assert(div)
    test.assertEquals(div.title, 'a')
    test.assertEquals(div.outerHTML, '<div title="a"></div>')

    await compute.txn(ctx, async () => {
      await compute.set(ctx, title, null)
    })
    test.assertEquals(div.title, '')
    test.assertEquals(div.outerHTML, '<div></div>')
  },

  async ['div on add'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const show = compute.state(false)
    const [stepCallback, stepCallbackHistory] = test.repeatMockWithHistory(
      6,
      (..._args: readonly unknown[]) => {},
    )

    const fragment = convey.if_(
      () =>
        convey.html.div({
          id: 'x',
          onAdd(event) {
            stepCallback('added', event.element)
            test.assertEquals(event.element.id, 'x')
            test.assertEquals(event.element.textContent, 'y')
            test.assertEquals(
              event.element.outerHTML,
              '<div id="x">y</div>',
            )

            convey.scopedDefer(event.ctx, () => {
              stepCallback('removed')
            })
          },

          content: 'y',
        }),
      () => null,
      show,
    )
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))

    stepCallback('start')
    await compute.txn(ctx, async () => {
      await compute.set(ctx, show, true)
    })
    stepCallback('shown')

    const div = body.querySelector('div')
    test.assert(div)

    await compute.txn(ctx, async () => {
      await compute.set(ctx, show, false)
    })
    stepCallback('hidden')

    test.assertDeepEquals(stepCallbackHistory, [
      ['start'],
      ['added', div],
      ['shown'],
      ['removed'],
      ['hidden'],
    ])
  },

  async ['button pure'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.html.button({ formMethod: 'get' })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const button = body.querySelector('button')
    test.assert(button)
    test.assertEquals(
      button.outerHTML,
      '<button formmethod="get"></button>',
    )
  },

  async ['button boolean false'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const disabled = compute.state(true)

    const fragment = convey.html.button({ disabled })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const button = body.querySelector('button')
    test.assert(button)
    test.assertEquals(button.disabled, true)
    test.assertEquals(button.outerHTML, '<button disabled=""></button>')

    await compute.txn(ctx, async () => {
      await compute.set(ctx, disabled, false)
    })
    test.assertEquals(button.disabled, false)
    test.assertEquals(button.outerHTML, '<button></button>')
  },
}

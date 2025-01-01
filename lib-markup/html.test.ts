/* eslint-disable @typescript-eslint/no-empty-function */
import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import * as compute from '@symbolize/lib-compute'
import * as styling from '@symbolize/lib-styling'
import * as stylingTest from '@symbolize/lib-styling/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['div advanced'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const custom = markup.defineCustom<
      unknown,
      {
        readonly title: compute.NodeOpt<string>
      }
    >((ctx, attrs) => {
      const countState = compute.state(0)

      return markup.html.div({
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
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const div = body.querySelector('div')
    test.assert(div)
    test.assertEquals(div.textContent, 'hello / 0')

    div.click()
    await markup.wait(ctx)
    test.assertEquals(div.textContent, 'hello / 1')

    await fragment.remove()
    div.click()
    await markup.wait(ctx)
    test.assertEquals(div.textContent, 'hello / 1')
  },

  async ['div pure'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const [clickCallback, clickCallbackHistory] =
      test.repeatMockWithHistory(1, (_event: Readonly<MouseEvent>) => {})

    const fragment = markup.html.div({
      ariaDisabled: true,
      id: 'x',
      onClick: clickCallback,

      content: 'y',
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
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
    await markup.wait(ctx)
    test.assertEquals(clickCallbackHistory.length, 1)
  },

  async ['div class names'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.html.div({
      className: ['x', 'y'],
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const div = body.querySelector('div')
    test.assert(div)
    test.assertEquals(div.className, 'x y')
  },

  async ['div state'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const x = compute.state('a')

    const fragment = markup.html.div({ id: x })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
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
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const title = compute.state<string | null>('a')

    const fragment = markup.html.div({ title })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
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
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const show = compute.state(false)
    const [stepCallback, stepCallbackHistory] = test.repeatMockWithHistory(
      6,
      (..._args: readonly unknown[]) => {},
    )

    const fragment = markup.if_(
      () =>
        markup.html.div({
          id: 'x',
          onAdd(event) {
            stepCallback('added', event.element)
            test.assertEquals(event.element.id, 'x')
            test.assertEquals(event.element.textContent, 'y')
            test.assertEquals(
              event.element.outerHTML,
              '<div id="x">y</div>',
            )

            markup.scopedDefer(event.ctx, () => {
              stepCallback('removed')
            })
          },

          content: 'y',
        }),
      () => null,
      show,
    )
    const body = await markupTest.addFragmentToBody(ctx, fragment)

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

  async ['div style'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    for (let i = 0; i < 2; i += 1) {
      const fragment = markup.html.div({
        style: [styling.background.size('contain')],
      })
      const body = await markupTest.addFragmentToBody(ctx, fragment)
      const div = body.querySelector('div')
      test.assert(div)
      test.assertDeepEquals([...div.classList.values()], ['a0'])
      test.assertDeepEquals(
        await Promise.all(
          [...ctx.markup.styleLayer.cssRules].map(async (item) =>
            stylingTest.formatCode(item.cssText),
          ),
        ),
        [
          stylingTest.dedent(`
            .a0 {
              background-size: contain;
            }
          `),
        ],
      )

      div.remove()
    }
  },

  async ['div computation style'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    for (let i = 0; i < 2; i += 1) {
      const x = compute.state<styling.Length | null>(styling.px(1))

      const fragment = markup.html.div({
        style: compute.map(
          (x) => (x !== null ? styling.padding.os(x) : null),
          x,
        ),
      })
      const body = await markupTest.addFragmentToBody(ctx, fragment)
      const div = body.querySelector('div')
      test.assert(div)
      test.assertDeepEquals([...div.classList.values()], ['a0'])
      test.assertDeepEquals(
        await Promise.all(
          [...ctx.markup.styleLayer.cssRules].map(async (item) =>
            stylingTest.formatCode(item.cssText),
          ),
        ),
        [
          stylingTest.dedent(`
            .a0 {
              padding-block-start: 1px;
            }
          `),
          stylingTest.dedent(`
            .a1 {
              padding-block-start: 2px;
            }
          `),
        ].slice(0, 1 + i),
      )

      await compute.txn(ctx, async () => {
        await compute.set(ctx, x, styling.px(2))
      })
      test.assertDeepEquals([...div.classList.values()], ['a1'])
      test.assertDeepEquals(
        await Promise.all(
          [...ctx.markup.styleLayer.cssRules].map(async (item) =>
            stylingTest.formatCode(item.cssText),
          ),
        ),
        [
          stylingTest.dedent(`
            .a0 {
              padding-block-start: 1px;
            }
          `),
          stylingTest.dedent(`
            .a1 {
              padding-block-start: 2px;
            }
          `),
        ],
      )

      await compute.txn(ctx, async () => {
        await compute.set(ctx, x, null)
      })
      test.assertDeepEquals([...div.classList.values()], [])
      test.assertDeepEquals(
        await Promise.all(
          [...ctx.markup.styleLayer.cssRules].map(async (item) =>
            stylingTest.formatCode(item.cssText),
          ),
        ),
        [
          stylingTest.dedent(`
            .a0 {
              padding-block-start: 1px;
            }
          `),
          stylingTest.dedent(`
            .a1 {
              padding-block-start: 2px;
            }
          `),
        ],
      )

      div.remove()
    }
  },

  async ['button pure'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.html.button({
      formMethod: 'get',
      type: 'submit',
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const button = body.querySelector('button')
    test.assert(button)
    test.assertEquals(
      button.outerHTML,
      '<button formmethod="get" type="submit"></button>',
    )
  },

  async ['button boolean false'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const disabled = compute.state(true)

    const fragment = markup.html.button({ disabled, type: 'button' })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const button = body.querySelector('button')
    test.assert(button)
    test.assertEquals(button.disabled, true)
    test.assertEquals(
      button.outerHTML,
      '<button disabled="" type="button"></button>',
    )

    await compute.txn(ctx, async () => {
      await compute.set(ctx, disabled, false)
    })
    test.assertEquals(button.disabled, false)
    test.assertEquals(button.outerHTML, '<button type="button"></button>')
  },

  async ['input checkbox pure'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.html.input({
      checked: true,
      type: 'checkbox',
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const input = body.querySelector('input')
    test.assert(input)
    test.assertEquals(
      input.outerHTML,
      '<input checked="" type="checkbox">',
    )
  },

  async ['input text pure'](
    ctx: compute.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.html.input({
      autocomplete: ['section-x', 'email'],
      type: 'text',
      value: 'x',
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const input = body.querySelector('input')
    test.assert(input)
    test.assertEquals(
      input.outerHTML,
      '<input autocomplete="section-x email" type="text" value="x">',
    )
  },
}

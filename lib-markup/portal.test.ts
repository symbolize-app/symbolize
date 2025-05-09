/* eslint-disable @typescript-eslint/no-empty-function */
import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import type * as dataflow from '@symbolize/lib-dataflow'
import * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['add and remove'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const [clickCallback, clickCallbackHistory] =
      test.repeatMockWithHistory(1, (_event: Readonly<MouseEvent>) => {})

    const div = ctx.markup.document.createElement('div')
    div.append('q')
    div.setAttribute('id', 'x')
    div.classList.add('s')

    div.click()
    await markup.wait(ctx)
    test.assertEquals(div.id, 'x')
    test.assertEquals(div.getAttribute('tabindex'), null)
    test.assertEquals(div.querySelectorAll('span').length, 0)
    test.assertEquals(div.textContent, 'q')
    test.assertDeepEquals([...div.classList.values()], ['s'])
    test.assertDeepEquals(clickCallbackHistory.length, 0)

    const fragment = markup.portal(div, {
      id: 'y',
      onClick: clickCallback,
      style: styling.background.color(
        styling.rgb(styling.pct(100), styling.pct(0), styling.pct(0)),
      ),
      tabIndex: 1,

      content: [
        '_',
        markup.if_(
          () =>
            markup.each(
              (x) => markup.html.span({ content: x }),
              (x) => x,
              ['a', 'b'],
            ),
          () => null,
          true,
        ),
      ],
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)

    div.click()
    await markup.wait(ctx)
    test.assertEquals(div.id, 'y')
    test.assertEquals(div.getAttribute('tabindex'), '1')
    test.assertEquals(div.querySelectorAll('span').length, 2)
    test.assertEquals(div.textContent, 'q_ab')
    test.assertDeepEquals([...div.classList.values()], ['s', 'a0'])
    test.assertDeepEquals(clickCallbackHistory.length, 1)

    await fragment.remove()

    div.click()
    await markup.wait(ctx)
    test.assertEquals(div.id, 'x')
    test.assertEquals(div.getAttribute('tabindex'), null)
    test.assertEquals(div.querySelectorAll('span').length, 0)
    test.assertEquals(div.textContent, 'q')
    test.assertDeepEquals([...div.classList.values()], ['s'])
    test.assertDeepEquals(clickCallbackHistory.length, 1)
  },
}

/* eslint-disable @typescript-eslint/no-empty-function */
import * as convey from '@/index.ts'
import * as conveyTest from '@/test.ts'
import type * as compute from '@symbolize/lib-compute'
import * as contrast from '@symbolize/lib-contrast'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['add and remove'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const [clickCallback, clickCallbackHistory] =
      test.repeatMockWithHistory(1, (_event: Readonly<MouseEvent>) => {})

    const div = ctx.convey.document.createElement('div')
    div.append('q')
    div.setAttribute('id', 'x')
    div.classList.add('s')

    div.click()
    await convey.wait(ctx)
    test.assertEquals(div.id, 'x')
    test.assertEquals(div.getAttribute('tabindex'), null)
    test.assertEquals(div.querySelectorAll('span').length, 0)
    test.assertEquals(div.textContent, 'q')
    test.assertDeepEquals([...div.classList.values()], ['s'])
    test.assertDeepEquals(clickCallbackHistory.length, 0)

    const fragment = convey.portal(div, {
      id: 'y',
      onClick: clickCallback,
      style: contrast.background.color(
        contrast.rgb(contrast.pct(100), contrast.pct(0), contrast.pct(0)),
      ),
      tabIndex: 1,

      content: [
        '_',
        convey.if_(
          () =>
            convey.each(
              (x) => convey.html.span({ content: x }),
              (x) => x,
              ['a', 'b'],
            ),
          () => null,
          true,
        ),
      ],
    })
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)

    div.click()
    await convey.wait(ctx)
    test.assertEquals(div.id, 'y')
    test.assertEquals(div.getAttribute('tabindex'), '1')
    test.assertEquals(div.querySelectorAll('span').length, 2)
    test.assertEquals(div.textContent, 'q_ab')
    test.assertDeepEquals([...div.classList.values()], ['s', 'a0'])
    test.assertDeepEquals(clickCallbackHistory.length, 1)

    await fragment.remove()

    div.click()
    await convey.wait(ctx)
    test.assertEquals(div.id, 'x')
    test.assertEquals(div.getAttribute('tabindex'), null)
    test.assertEquals(div.querySelectorAll('span').length, 0)
    test.assertEquals(div.textContent, 'q')
    test.assertDeepEquals([...div.classList.values()], ['s'])
    test.assertDeepEquals(clickCallbackHistory.length, 1)
  },
}

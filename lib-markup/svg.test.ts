import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import type * as dataflow from '@symbolize/lib-dataflow'
import type * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['rect pure'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.svg.svg({
      content: markup.svg.rect({
        height: 3,
        width: 2,
        x: 0,
        y: 1,
      }),
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const svg = body.querySelector('svg')
    test.assert(svg)
    test.assertEquals(
      svg.outerHTML,
      '<svg><rect height="3" width="2" x="0" y="1"></rect></svg>',
    )
  },

  async ['svg pure'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.svg.svg({
      ariaDisabled: true,
      preserveAspectRatio: markup.svgPreserveAspectRatio({
        align: 'xMaxYMax',
        mode: 'slice',
      }),
      viewBox: [1, 2, 3, 4],
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const svg = body.querySelector('svg')
    test.assert(svg)
    test.assertEquals(
      svg.outerHTML,
      '<svg aria-disabled="true" preserveAspectRatio="xMaxYMax slice" viewBox="1 2 3 4"></svg>',
    )
  },
}

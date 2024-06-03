import * as convey from '@/index.ts'
import * as conveyTest from '@/test.ts'
import type * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['rect pure'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.svg.svg({
      content: convey.svg.rect({
        height: 3,
        width: 2,
        x: 0,
        y: 1,
      }),
    })
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    const svg = body.querySelector('svg')
    test.assert(svg)
    test.assertEquals(
      svg.outerHTML,
      '<svg><rect height="3" width="2" x="0" y="1"></rect></svg>',
    )
  },

  async ['svg pure'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.svg.svg({
      ariaDisabled: true,
      preserveAspectRatio: convey.svgPreserveAspectRatio({
        align: 'xMaxYMax',
        mode: 'slice',
      }),
      viewBox: [1, 2, 3, 4],
    })
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    const svg = body.querySelector('svg')
    test.assert(svg)
    test.assertEquals(
      svg.outerHTML,
      '<svg aria-disabled="true" preserveAspectRatio="xMaxYMax slice" viewBox="1 2 3 4"></svg>',
    )
  },
}

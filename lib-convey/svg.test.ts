import * as convey from '@/index.ts'
import type * as compute from '@intertwine/lib-compute'
import * as test from '@intertwine/lib-test'
import arrayFromAsync from 'core-js-pure/actual/array/from-async'

export const url = import.meta.url

export const tests = {
  async ['rect pure'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.svg.svg({
      content: convey.svg.rect({
        height: 3,
        width: 2,
        x: 0,
        y: 1,
      }),
    })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const svg = body.querySelector('svg')
    test.assert(svg)
    test.assertEquals(
      svg.outerHTML,
      '<svg><rect height="3" width="2" x="0" y="1"></rect></svg>',
    )
  },

  async ['svg pure'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.svg.svg({
      preserveAspectRatio: convey.svgPreserveAspectRatio({
        align: 'xMaxYMax',
        mode: 'slice',
      }),
      viewBox: [1, 2, 3, 4],
    })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const svg = body.querySelector('svg')
    test.assert(svg)
    test.assertEquals(
      svg.outerHTML,
      '<svg preserveAspectRatio="xMaxYMax slice" viewBox="1 2 3 4"></svg>',
    )
  },
}

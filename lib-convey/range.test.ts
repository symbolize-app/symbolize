import * as convey from '@/index.ts'
import type * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'
import * as test from '@intertwine/lib-test'
import arrayFromAsync from 'core-js-pure/actual/array/from-async'

export const url = import.meta.url

export const tests = {
  async ['range'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
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
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const body = ctx.convey.document.body
    body.append(
      ...(await arrayFromAsync(convey.toFragment(['a', 'b']).add(ctx))),
    )
    test.assertEquals(body.childNodes.length, 2)
    test.assertEquals(body.textContent, 'ab')
  },
}

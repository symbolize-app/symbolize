import * as convey from '@/index.ts'
import * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'
import * as test from '@intertwine/lib-test'
import arrayFromAsync from 'core-js-pure/actual/array/from-async'

export const url = import.meta.url

export const tests = {
  async ['text pure'](
    ctx: compute.Context & contrast.Context & convey.Context,
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
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const body = ctx.convey.document.body
    body.append(
      ...(await arrayFromAsync(convey.toFragment('hello').add(ctx))),
    )
    test.assertEquals(body.textContent, 'hello')
  },

  async ['text state'](
    ctx: compute.Context & contrast.Context & convey.Context,
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
}

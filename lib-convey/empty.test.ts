import * as convey from '@/index.ts'
import * as conveyTest from '@/test.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['empty'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.empty()
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)
  },

  async ['empty opt'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.toFragment(null)
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)
  },
}

import * as hypertext from '@/index.ts'
import * as hypertextTest from '@/test.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['empty'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const fragment = hypertext.empty()
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)
  },

  async ['empty opt'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const fragment = hypertext.toFragment(null)
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
    test.assertEquals(body.childNodes.length, 0)
  },
}

import * as convey from '@/index.ts'
import type * as compute from '@intertwine/lib-compute'
import * as test from '@intertwine/lib-test'
import arrayFromAsync from 'core-js-pure/actual/array/from-async'

export const url = import.meta.url

export const tests = {
  async ['empty'](ctx: compute.Context & convey.Context): Promise<void> {
    test.assertDeepEquals(
      await arrayFromAsync(convey.empty().add(ctx)),
      [],
    )
  },

  async ['empty opt'](
    ctx: compute.Context & convey.Context,
  ): Promise<void> {
    test.assertDeepEquals(
      await arrayFromAsync(convey.toFragment(null).add(ctx)),
      [],
    )
  },
}

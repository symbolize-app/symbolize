import * as convey from '@/index.ts'
import * as conveyTest from '@/test.ts'
import type * as compute from '@intertwine/lib-compute'
import * as contrast from '@intertwine/lib-contrast'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['math pure'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.math.math({
      ariaDisabled: true,
      display: 'inline',
    })
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math aria-disabled="true" display="inline"></math>',
    )
  },

  async ['mi pure'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.math.math({
      content: convey.math.mi({
        mathVariant: 'normal',
      }),
    })
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math><mi mathvariant="normal"></mi></math>',
    )
  },

  async ['mo pure'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.math.math({
      content: convey.math.mo({
        maxSize: contrast.px(11),
        operatorForm: 'prefix',
        stretchy: true,
      }),
    })
    const body = await conveyTest.addFragmentToBody(ctx, fragment)
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math><mo maxsize="11px" form="prefix" stretchy="true"></mo></math>',
    )
  },
}

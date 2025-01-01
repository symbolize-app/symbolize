import * as hypertext from '@/index.ts'
import * as hypertextTest from '@/test.ts'
import type * as compute from '@symbolize/lib-compute'
import * as contrast from '@symbolize/lib-contrast'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['math pure'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const fragment = hypertext.math.math({
      ariaDisabled: true,
      display: 'inline',
    })
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math aria-disabled="true" display="inline"></math>',
    )
  },

  async ['mi pure'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const fragment = hypertext.math.math({
      content: hypertext.math.mi({
        mathVariant: 'normal',
      }),
    })
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math><mi mathvariant="normal"></mi></math>',
    )
  },

  async ['mo pure'](
    ctx: compute.Context & contrast.Context & hypertext.Context,
  ): Promise<void> {
    const fragment = hypertext.math.math({
      content: hypertext.math.mo({
        maxSize: contrast.px(11),
        operatorForm: 'prefix',
        stretchy: true,
      }),
    })
    const body = await hypertextTest.addFragmentToBody(ctx, fragment)
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math><mo maxsize="11px" form="prefix" stretchy="true"></mo></math>',
    )
  },
}

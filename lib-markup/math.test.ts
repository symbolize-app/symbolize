import * as markup from '@/index.ts'
import * as markupTest from '@/test.ts'
import type * as dataflow from '@symbolize/lib-dataflow'
import * as styling from '@symbolize/lib-styling'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['math pure'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.math.math({
      ariaDisabled: true,
      display: 'inline',
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math aria-disabled="true" display="inline"></math>',
    )
  },

  async ['mi pure'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.math.math({
      content: markup.math.mi({
        mathVariant: 'normal',
      }),
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math><mi mathvariant="normal"></mi></math>',
    )
  },

  async ['mo pure'](
    ctx: dataflow.Context & markup.Context & styling.Context,
  ): Promise<void> {
    const fragment = markup.math.math({
      content: markup.math.mo({
        maxSize: styling.px(11),
        operatorForm: 'prefix',
        stretchy: true,
      }),
    })
    const body = await markupTest.addFragmentToBody(ctx, fragment)
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math><mo maxsize="11px" form="prefix" stretchy="true"></mo></math>',
    )
  },
}

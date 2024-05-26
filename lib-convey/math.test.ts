import * as convey from '@/index.ts'
import type * as compute from '@intertwine/lib-compute'
import * as contrast from '@intertwine/lib-contrast'
import * as test from '@intertwine/lib-test'
import arrayFromAsync from 'core-js-pure/actual/array/from-async'

export const url = import.meta.url

export const tests = {
  async ['math pure'](
    ctx: compute.Context & contrast.Context & convey.Context,
  ): Promise<void> {
    const fragment = convey.math.math({
      ariaDisabled: true,
      display: 'inline',
    })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
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
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
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
        form: 'prefix',
        maxSize: contrast.px(11),
        stretchy: true,
      }),
    })
    const body = ctx.convey.document.body
    body.append(...(await arrayFromAsync(fragment.add(ctx))))
    const math = body.querySelector('math')
    test.assert(math)
    test.assertEquals(
      math.outerHTML,
      '<math><mo form="prefix" maxsize="11px" stretchy="true"></mo></math>',
    )
  },
}

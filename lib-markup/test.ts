import * as markup from '@/index.ts'
import type * as dataflow from '@symbolize/lib-dataflow'
import type * as styling from '@symbolize/lib-styling'

export const url = import.meta.url

export async function addFragmentToBody(
  ctx: dataflow.Context & markup.Context & styling.Context,
  fragment: markup.Fragment,
): Promise<HTMLElement> {
  const body = ctx.markup.document.body
  await markup.portal(body, { content: fragment }).add(ctx)
  return body
}

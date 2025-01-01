import * as markup from '@/index.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as styling from '@symbolize/lib-styling'

export const url = import.meta.url

export async function addFragmentToBody(
  ctx: compute.Context & markup.Context & styling.Context,
  fragment: markup.Fragment,
): Promise<HTMLElement> {
  const body = ctx.markup.document.body
  await markup.portal(body, { content: fragment }).add(ctx)
  return body
}

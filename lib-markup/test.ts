import * as markup from '@/index.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'

export const url = import.meta.url

export async function addFragmentToBody(
  ctx: compute.Context & contrast.Context & markup.Context,
  fragment: markup.Fragment,
): Promise<HTMLElement> {
  const body = ctx.markup.document.body
  await markup.portal(body, { content: fragment }).add(ctx)
  return body
}

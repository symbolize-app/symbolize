import * as hypertext from '@/index.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'

export const url = import.meta.url

export async function addFragmentToBody(
  ctx: compute.Context & contrast.Context & hypertext.Context,
  fragment: hypertext.Fragment,
): Promise<HTMLElement> {
  const body = ctx.hypertext.document.body
  await hypertext.portal(body, { content: fragment }).add(ctx)
  return body
}

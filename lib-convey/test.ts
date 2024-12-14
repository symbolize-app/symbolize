import * as convey from '@/index.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'

export const url = import.meta.url

export async function addFragmentToBody(
  ctx: compute.Context & contrast.Context & convey.Context,
  fragment: convey.Fragment,
): Promise<HTMLElement> {
  const body = ctx.convey.document.body
  await convey.portal(body, { content: fragment }).add(ctx)
  return body
}

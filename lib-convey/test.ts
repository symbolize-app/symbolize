import type * as convey from '@/index.ts'
import type * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'

export const url = import.meta.url

export async function addFragmentToBody(
  ctx: compute.Context & contrast.Context & convey.Context,
  fragment: convey.Fragment,
): Promise<HTMLElement> {
  const body = ctx.convey.document.body
  await fragment.add(ctx)
  body.append(...fragment.nodes())
  return body
}

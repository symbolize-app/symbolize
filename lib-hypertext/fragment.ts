import type * as hypertextContext from '@/context.ts'
import * as hypertextEmpty from '@/empty.ts'
import * as hypertextMarker from '@/marker.ts'
import * as hypertextRange from '@/range.ts'
import * as hypertextText from '@/text.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'

export interface Fragment<CustomContext = unknown> {
  readonly [hypertextMarker.fragmentMarker]: null
  add(
    ctx: compute.Context &
      contrast.Context &
      CustomContext &
      hypertextContext.Context,
  ): Promise<void>
  nodes(): IterableIterator<globalThis.Node>
  remove(): Promise<void>
}

export type FragmentOpt<CustomContext = unknown> =
  | compute.NodeOpt<string>
  | Fragment<CustomContext>
  | readonly FragmentOpt<CustomContext>[]
  | null

export function toFragment<CustomContext = unknown>(
  fragment: FragmentOpt<CustomContext>,
): Fragment {
  if (typeof fragment === 'object') {
    if (fragment === null) {
      return hypertextEmpty.empty()
    } else if (isReadonlyArray(fragment)) {
      return hypertextRange.range({ content: fragment })
    } else if (hypertextMarker.fragmentMarker in fragment) {
      return fragment
    }
  }
  return hypertextText.text({ content: fragment })
}

function isReadonlyArray(arg: unknown): arg is readonly unknown[] {
  return Array.isArray(arg)
}

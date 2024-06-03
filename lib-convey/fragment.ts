import type * as conveyContext from '@/context.ts'
import * as conveyEmpty from '@/empty.ts'
import * as conveyMarker from '@/marker.ts'
import * as conveyRange from '@/range.ts'
import * as conveyText from '@/text.ts'
import type * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'

export interface Fragment<CustomContext = unknown> {
  readonly [conveyMarker.fragmentMarker]: null
  add(
    ctx: compute.Context &
      contrast.Context &
      conveyContext.Context &
      CustomContext,
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
      return conveyEmpty.empty()
    } else if (isReadonlyArray(fragment)) {
      return conveyRange.range({ content: fragment })
    } else if (conveyMarker.fragmentMarker in fragment) {
      return fragment
    }
  }
  return conveyText.text({ content: fragment })
}

function isReadonlyArray(arg: unknown): arg is readonly unknown[] {
  return Array.isArray(arg)
}

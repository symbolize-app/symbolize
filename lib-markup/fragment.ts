import type * as markupContext from '@/context.ts'
import * as markupEmpty from '@/empty.ts'
import * as markupMarker from '@/marker.ts'
import * as markupRange from '@/range.ts'
import * as markupText from '@/text.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as styling from '@symbolize/lib-styling'

export interface Fragment<CustomContext = unknown> {
  readonly [markupMarker.fragmentMarker]: null
  add(
    ctx: compute.Context &
      CustomContext &
      markupContext.Context &
      styling.Context,
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
      return markupEmpty.empty()
    } else if (isReadonlyArray(fragment)) {
      return markupRange.range({ content: fragment })
    } else if (markupMarker.fragmentMarker in fragment) {
      return fragment
    }
  }
  return markupText.text({ content: fragment })
}

function isReadonlyArray(arg: unknown): arg is readonly unknown[] {
  return Array.isArray(arg)
}

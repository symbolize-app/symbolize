import type * as hypertextContext from '@/context.ts'
import * as hypertextFragment from '@/fragment.ts'
import * as hypertextMarker from '@/marker.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'

export function range<CustomContext = unknown>(attrs: {
  readonly content: readonly hypertextFragment.FragmentOpt<CustomContext>[]
}): hypertextFragment.Fragment<CustomContext> {
  return new Range(attrs.content)
}

class Range<CustomContext = unknown>
  implements hypertextFragment.Fragment<CustomContext>
{
  readonly [hypertextMarker.fragmentMarker]: null = null
  private mutableFragments:
    | hypertextFragment.Fragment<CustomContext>[]
    | null = null

  constructor(
    private readonly content: readonly hypertextFragment.FragmentOpt<CustomContext>[],
  ) {}

  async add(
    ctx: compute.Context &
      contrast.Context &
      CustomContext &
      hypertextContext.Context,
  ): Promise<void> {
    this.mutableFragments = this.content.map(hypertextFragment.toFragment)
    for (const fragment of this.mutableFragments) {
      await fragment.add(ctx)
    }
  }

  *nodes(): IterableIterator<Node> {
    if (this.mutableFragments) {
      for (const fragment of this.mutableFragments) {
        for (const node of fragment.nodes()) {
          yield node
        }
      }
    }
  }

  async remove(): Promise<void> {
    if (this.mutableFragments) {
      for (const fragment of this.mutableFragments) {
        await fragment.remove()
      }
      this.mutableFragments = null
    }
  }
}

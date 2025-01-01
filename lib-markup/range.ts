import type * as markupContext from '@/context.ts'
import * as markupFragment from '@/fragment.ts'
import * as markupMarker from '@/marker.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'

export function range<CustomContext = unknown>(attrs: {
  readonly content: readonly markupFragment.FragmentOpt<CustomContext>[]
}): markupFragment.Fragment<CustomContext> {
  return new Range(attrs.content)
}

class Range<CustomContext = unknown>
  implements markupFragment.Fragment<CustomContext>
{
  readonly [markupMarker.fragmentMarker]: null = null
  private mutableFragments:
    | markupFragment.Fragment<CustomContext>[]
    | null = null

  constructor(
    private readonly content: readonly markupFragment.FragmentOpt<CustomContext>[],
  ) {}

  async add(
    ctx: compute.Context &
      contrast.Context &
      CustomContext &
      markupContext.Context,
  ): Promise<void> {
    this.mutableFragments = this.content.map(markupFragment.toFragment)
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

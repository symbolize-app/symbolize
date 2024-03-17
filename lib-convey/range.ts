import type * as conveyContext from '@/context.ts'
import * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import type * as compute from '@intertwine/lib-compute'

export function range<CustomContext = unknown>(attrs: {
  readonly content: readonly conveyFragment.FragmentOpt<CustomContext>[]
}): conveyFragment.Fragment<CustomContext> {
  return new Range(attrs.content)
}

class Range<CustomContext = unknown>
  implements conveyFragment.Fragment<CustomContext>
{
  readonly [conveyMarker.fragmentMarker]: null = null
  private mutableFragments:
    | conveyFragment.Fragment<CustomContext>[]
    | null = null

  constructor(
    private readonly content: readonly conveyFragment.FragmentOpt<CustomContext>[],
  ) {}

  async *add(
    ctx: compute.Context & conveyContext.Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    this.mutableFragments = this.content.map(conveyFragment.toFragment)
    for (const fragment of this.mutableFragments) {
      for await (const node of fragment.add(ctx)) {
        yield node
      }
    }
  }

  async remove(): Promise<void> {
    if (this.mutableFragments) {
      for (const fragment of this.mutableFragments) {
        await fragment.remove()
      }
    }
  }
}

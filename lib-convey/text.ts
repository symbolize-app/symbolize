import type * as conveyContext from '@/context.ts'
import type * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import * as compute from '@intertwine/lib-compute'

export function text(attrs: {
  readonly content: compute.ComputationOpt<string>
}): conveyFragment.Fragment {
  return new Text(attrs.content)
}

class Text implements conveyFragment.Fragment {
  readonly [conveyMarker.fragmentMarker]: null = null
  private mutableEffect: compute.Computation<void> | null = null

  constructor(private readonly content: compute.ComputationOpt<string>) {}

  async *add(
    ctx: compute.Context & conveyContext.Context,
  ): AsyncIterableIterator<Node> {
    const mutableNode = ctx.convey.document.createTextNode('')
    this.mutableEffect = await compute.effect((value) => {
      mutableNode.textContent = value
    }, this.content)
    yield mutableNode
  }

  // eslint-disable-next-line @typescript-eslint/require-await -- override
  async remove(): Promise<void> {
    if (this.mutableEffect !== null) {
      compute.unsubscribe(this.mutableEffect)
    }
  }
}

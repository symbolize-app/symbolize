import type * as conveyContext from '@/context.ts'
import type * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'

export function text(attrs: {
  readonly content: compute.NodeOpt<string>
}): conveyFragment.Fragment {
  return new TextFragment(attrs.content)
}

class TextFragment implements conveyFragment.Fragment {
  readonly [conveyMarker.fragmentMarker]: null = null
  private mutableNode: Text | null = null
  private mutableSub: compute.Computation<void> | null = null

  constructor(private readonly content: compute.NodeOpt<string>) {}

  async add(
    ctx: compute.Context & contrast.Context & conveyContext.Context,
  ): Promise<void> {
    this.mutableNode = ctx.convey.document.createTextNode('')
    this.mutableSub = await compute.effect((value) => {
      if (this.mutableNode) {
        this.mutableNode.textContent = value
      }
    }, this.content)
  }

  *nodes(): IterableIterator<Node> {
    if (this.mutableNode) {
      yield this.mutableNode
    }
  }

  // eslint-disable-next-line @typescript-eslint/require-await -- override
  async remove(): Promise<void> {
    if (this.mutableSub) {
      compute.unsubscribe(this.mutableSub)
      this.mutableSub = null
    }
    this.mutableNode = null
  }
}

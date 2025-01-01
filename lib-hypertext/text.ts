import type * as hypertextContext from '@/context.ts'
import type * as hypertextFragment from '@/fragment.ts'
import * as hypertextMarker from '@/marker.ts'
import * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'

export function text(attrs: {
  readonly content: compute.NodeOpt<string>
}): hypertextFragment.Fragment {
  return new TextFragment(attrs.content)
}

class TextFragment implements hypertextFragment.Fragment {
  readonly [hypertextMarker.fragmentMarker]: null = null
  private mutableNode: Text | null = null
  private mutableSub: compute.Computation<void> | null = null

  constructor(private readonly content: compute.NodeOpt<string>) {}

  async add(
    ctx: compute.Context & contrast.Context & hypertextContext.Context,
  ): Promise<void> {
    this.mutableNode = ctx.hypertext.document.createTextNode('')
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

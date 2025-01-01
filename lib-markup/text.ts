import type * as markupContext from '@/context.ts'
import type * as markupFragment from '@/fragment.ts'
import * as markupMarker from '@/marker.ts'
import * as dataflow from '@symbolize/lib-dataflow'
import type * as styling from '@symbolize/lib-styling'

export function text(attrs: {
  readonly content: dataflow.NodeOpt<string>
}): markupFragment.Fragment {
  return new TextFragment(attrs.content)
}

class TextFragment implements markupFragment.Fragment {
  readonly [markupMarker.fragmentMarker]: null = null
  private mutableNode: Text | null = null
  private mutableSub: dataflow.Computation<void> | null = null

  constructor(private readonly content: dataflow.NodeOpt<string>) {}

  async add(
    ctx: dataflow.Context & markupContext.Context & styling.Context,
  ): Promise<void> {
    this.mutableNode = ctx.markup.document.createTextNode('')
    this.mutableSub = await dataflow.effect((value) => {
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
      dataflow.unsubscribe(this.mutableSub)
      this.mutableSub = null
    }
    this.mutableNode = null
  }
}

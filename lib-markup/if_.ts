import type * as markupContext from '@/context.ts'
import * as markupElement from '@/element.ts'
import * as markupFragment from '@/fragment.ts'
import * as markupMarker from '@/marker.ts'
import * as dataflow from '@symbolize/lib-dataflow'
import type * as styling from '@symbolize/lib-styling'

type Falsy = '' | 0 | 0n | false | null | undefined

export function if_<CustomContext, Value>(
  ifBranch: (
    value: dataflow.Computation<Exclude<Value, Falsy>>,
  ) => markupFragment.FragmentOpt<CustomContext>,
  elseBranch: () => markupFragment.FragmentOpt<CustomContext>,
  condition: dataflow.NodeOpt<Value>,
): markupFragment.Fragment<CustomContext> {
  return new If_(ifBranch, elseBranch, condition)
}

class If_<Value, CustomContext = unknown>
  implements markupFragment.Fragment<CustomContext>
{
  readonly [markupMarker.fragmentMarker]: null = null
  private mutableEndComment: Comment | null = null
  private mutableFragment: markupFragment.Fragment<CustomContext> | null =
    null
  private mutableInnerNodes: readonly Node[] | null = null
  private mutableStartComment: Comment | null = null
  private mutableSub: dataflow.Computation<void> | null = null

  constructor(
    private readonly ifBranch: (
      value: dataflow.Computation<Exclude<Value, Falsy>>,
    ) => markupFragment.FragmentOpt<CustomContext>,
    private readonly elseBranch: () => markupFragment.FragmentOpt<CustomContext>,
    private readonly condition: dataflow.NodeOpt<Value>,
  ) {}

  async add(
    ctx: CustomContext &
      dataflow.Context &
      markupContext.Context &
      styling.Context,
  ): Promise<void> {
    this.mutableStartComment = ctx.markup.document.createComment('')
    this.mutableEndComment = ctx.markup.document.createComment('')

    let ifResult:
      | [
          dataflow.Mutation<Exclude<Value, Falsy>>,
          markupFragment.Fragment<CustomContext>,
        ]
      | null = null
    let elseFragment: markupFragment.Fragment<CustomContext> | null = null

    this.mutableSub = await dataflow.effect(async (value) => {
      if (value) {
        const truthyValue = value as Exclude<Value, Falsy>
        if (elseFragment) {
          await elseFragment.remove()
          elseFragment = null
        }
        if (!ifResult) {
          const ifState = dataflow.state(truthyValue)
          const ifFragment = markupFragment.toFragment(
            this.ifBranch(ifState),
          )
          ifResult = [ifState, ifFragment]
          const mutableIfNodes: Node[] = []
          await ifFragment.add(ctx)
          if (this.mutableStartComment && this.mutableEndComment) {
            for (const node of ifFragment.nodes()) {
              mutableIfNodes.push(node)
            }
            this.mutableInnerNodes = markupElement.replaceBetween(
              this.mutableStartComment,
              this.mutableEndComment,
              mutableIfNodes,
            )
          }
          this.mutableFragment = ifFragment
        } else {
          const [ifState] = ifResult
          await dataflow.txn(ctx, async () => {
            await dataflow.set(ctx, ifState, truthyValue)
          })
        }
      } else {
        if (ifResult) {
          const [, ifFragment] = ifResult
          await ifFragment.remove()
          ifResult = null
        }
        if (!elseFragment) {
          elseFragment = markupFragment.toFragment(this.elseBranch())
          const mutableElseNodes: Node[] = []
          await elseFragment.add(ctx)
          if (this.mutableStartComment && this.mutableEndComment) {
            for (const node of elseFragment.nodes()) {
              mutableElseNodes.push(node)
            }
            this.mutableInnerNodes = markupElement.replaceBetween(
              this.mutableStartComment,
              this.mutableEndComment,
              mutableElseNodes,
            )
          }
          this.mutableFragment = elseFragment
        }
      }
    }, this.condition)
  }

  *nodes(): IterableIterator<Node> {
    if (
      this.mutableStartComment &&
      this.mutableEndComment &&
      this.mutableInnerNodes
    ) {
      yield this.mutableStartComment
      for (const node of this.mutableInnerNodes) {
        yield node
      }
      yield this.mutableEndComment
    }
  }

  async remove(): Promise<void> {
    if (this.mutableFragment) {
      await this.mutableFragment.remove()
      this.mutableFragment = null
    }

    if (this.mutableSub) {
      dataflow.unsubscribe(this.mutableSub)
      this.mutableSub = null
    }

    this.mutableStartComment = null
    this.mutableEndComment = null
    this.mutableInnerNodes = null
  }
}

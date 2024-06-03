import type * as conveyContext from '@/context.ts'
import * as conveyElement from '@/element.ts'
import * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'

type Falsy = '' | 0 | 0n | false | null | undefined

export function if_<CustomContext, Value>(
  ifBranch: (
    value: compute.Computation<Exclude<Value, Falsy>>,
  ) => conveyFragment.FragmentOpt<CustomContext>,
  elseBranch: () => conveyFragment.FragmentOpt<CustomContext>,
  condition: compute.NodeOpt<Value>,
): conveyFragment.Fragment<CustomContext> {
  return new If_(ifBranch, elseBranch, condition)
}

class If_<Value, CustomContext = unknown>
  implements conveyFragment.Fragment<CustomContext>
{
  readonly [conveyMarker.fragmentMarker]: null = null
  private mutableEndComment: Comment | null = null
  private mutableFragment: conveyFragment.Fragment<CustomContext> | null =
    null
  private mutableInnerNodes: readonly Node[] | null = null
  private mutableStartComment: Comment | null = null
  private mutableSub: compute.Computation<void> | null = null

  constructor(
    private readonly ifBranch: (
      value: compute.Computation<Exclude<Value, Falsy>>,
    ) => conveyFragment.FragmentOpt<CustomContext>,
    private readonly elseBranch: () => conveyFragment.FragmentOpt<CustomContext>,
    private readonly condition: compute.NodeOpt<Value>,
  ) {}

  async add(
    ctx: compute.Context &
      contrast.Context &
      conveyContext.Context &
      CustomContext,
  ): Promise<void> {
    this.mutableStartComment = ctx.convey.document.createComment('')
    this.mutableEndComment = ctx.convey.document.createComment('')

    let ifResult:
      | [
          compute.Mutation<Exclude<Value, Falsy>>,
          conveyFragment.Fragment<CustomContext>,
        ]
      | null = null
    let elseFragment: conveyFragment.Fragment<CustomContext> | null = null

    this.mutableSub = await compute.effect(async (value) => {
      if (value) {
        const truthyValue = value as Exclude<Value, Falsy>
        if (elseFragment) {
          await elseFragment.remove()
          elseFragment = null
        }
        if (!ifResult) {
          const ifState = compute.state(truthyValue)
          const ifFragment = conveyFragment.toFragment(
            this.ifBranch(ifState),
          )
          ifResult = [ifState, ifFragment]
          const mutableIfNodes: Node[] = []
          await ifFragment.add(ctx)
          if (this.mutableStartComment && this.mutableEndComment) {
            for (const node of ifFragment.nodes()) {
              mutableIfNodes.push(node)
            }
            this.mutableInnerNodes = conveyElement.replaceBetween(
              this.mutableStartComment,
              this.mutableEndComment,
              mutableIfNodes,
            )
          }
          this.mutableFragment = ifFragment
        } else {
          const [ifState] = ifResult
          await compute.txn(ctx, async () => {
            await compute.set(ctx, ifState, truthyValue)
          })
        }
      } else {
        if (ifResult) {
          const [, ifFragment] = ifResult
          await ifFragment.remove()
          ifResult = null
        }
        if (!elseFragment) {
          elseFragment = conveyFragment.toFragment(this.elseBranch())
          const mutableElseNodes: Node[] = []
          await elseFragment.add(ctx)
          if (this.mutableStartComment && this.mutableEndComment) {
            for (const node of elseFragment.nodes()) {
              mutableElseNodes.push(node)
            }
            this.mutableInnerNodes = conveyElement.replaceBetween(
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
      compute.unsubscribe(this.mutableSub)
      this.mutableSub = null
    }

    this.mutableStartComment = null
    this.mutableEndComment = null
    this.mutableInnerNodes = null
  }
}

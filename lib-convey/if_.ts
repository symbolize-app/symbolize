import type * as conveyContext from '@/context.ts'
import * as conveyElement from '@/element.ts'
import * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import * as compute from '@intertwine/lib-compute'

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
  private mutableFragment: conveyFragment.Fragment<CustomContext> | null =
    null
  private mutableSub: compute.Computation<void> | null = null

  constructor(
    private readonly ifBranch: (
      value: compute.Computation<Exclude<Value, Falsy>>,
    ) => conveyFragment.FragmentOpt<CustomContext>,
    private readonly elseBranch: () => conveyFragment.FragmentOpt<CustomContext>,
    private readonly condition: compute.NodeOpt<Value>,
  ) {}

  async *add(
    ctx: compute.Context & conveyContext.Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    const startComment = ctx.convey.document.createComment('')
    const endComment = ctx.convey.document.createComment('')
    let innerNodes = null as readonly Node[] | null

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
          for await (const node of ifFragment.add(ctx)) {
            mutableIfNodes.push(node)
          }
          innerNodes = conveyElement.replaceBetween(
            startComment,
            endComment,
            mutableIfNodes,
          )
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
          for await (const node of elseFragment.add(ctx)) {
            mutableElseNodes.push(node)
          }
          innerNodes = conveyElement.replaceBetween(
            startComment,
            endComment,
            mutableElseNodes,
          )
          this.mutableFragment = elseFragment
        }
      }
    }, this.condition)

    if (!innerNodes) {
      throw new Error('No fragment set')
    }
    yield startComment
    for await (const node of innerNodes) {
      yield node
    }
    yield endComment
  }

  async remove(): Promise<void> {
    if (this.mutableFragment) {
      await this.mutableFragment.remove()
    }

    if (this.mutableSub !== null) {
      compute.unsubscribe(this.mutableSub)
    }
  }
}

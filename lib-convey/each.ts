import type * as conveyContext from '@/context.ts'
import * as conveyElement from '@/element.ts'
import * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'

export function each<CustomContext, Key, Value>(
  transform: (
    value: compute.Computation<Value>,
  ) => conveyFragment.FragmentOpt<CustomContext>,
  key: (value: Value, index: number) => Key,
  items: compute.NodeOpt<readonly Value[]>,
): conveyFragment.Fragment<CustomContext> {
  return new Each(transform, key, items)
}

class Each<Key, Value, CustomContext = unknown>
  implements conveyFragment.Fragment<CustomContext>
{
  readonly [conveyMarker.fragmentMarker]: null = null
  private mutableFragments:
    | [number | null, conveyFragment.Fragment<CustomContext>][]
    | null = null
  private mutableSub: compute.Computation<void> | null = null

  constructor(
    private readonly transform: (
      value: compute.Computation<Value>,
    ) => conveyFragment.FragmentOpt<CustomContext>,
    private readonly key: (value: Value, index: number) => Key,
    private readonly items: compute.NodeOpt<readonly Value[]>,
  ) {}

  async *add(
    ctx: compute.Context &
      contrast.Context &
      conveyContext.Context &
      CustomContext,
  ): AsyncIterableIterator<Node> {
    const startComment = ctx.convey.document.createComment('')
    const mutableInnerComments: Comment[] = []
    const endComment = ctx.convey.document.createComment('')
    let innerNodes = null as readonly Node[] | null

    let previousResults = new Map<
      Key,
      [
        number,
        Value,
        compute.Mutation<Value>,
        conveyFragment.Fragment<CustomContext>,
      ]
    >()

    this.mutableSub = await compute.effect(async (items) => {
      let rebuild = false
      const newResults: typeof previousResults = new Map()
      const mutableNewFragments: [
        number | null,
        conveyFragment.Fragment<CustomContext>,
      ][] = []

      for (let i = 0; i < items.length; i++) {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
        const value = items[i]!
        const key = this.key(value, i)
        const oldResult = previousResults.get(key)
        let newResult: [
          number,
          Value,
          compute.Mutation<Value>,
          conveyFragment.Fragment<CustomContext>,
        ]
        if (oldResult) {
          previousResults.delete(key)
          const [oldIndex, oldValue, state, fragment] = oldResult
          if (oldValue !== value) {
            await compute.txn(ctx, async () => {
              await compute.set(ctx, state, value)
            })
          }
          if (oldIndex !== i) {
            rebuild = true
          }
          mutableNewFragments.push([oldIndex, fragment])
          newResult = [i, value, state, fragment]
        } else {
          rebuild = true
          const state = compute.state(value)
          const fragment = conveyFragment.toFragment(this.transform(state))
          mutableNewFragments.push([null, fragment])
          newResult = [i, value, state, fragment]
        }
        if (newResults.has(key)) {
          throw new Error(`Duplicate key: ${JSON.stringify(key)}`)
        }
        newResults.set(key, newResult)
      }

      this.mutableFragments = mutableNewFragments

      for (const [, , , fragment] of previousResults.values()) {
        rebuild = true
        await fragment.remove()
      }

      if (rebuild) {
        const mutableItemNodes: Node[][] = []
        for (const [oldIndex, fragment] of mutableNewFragments) {
          const mutableNodes: Node[] = []
          if (oldIndex !== null) {
            const start =
              oldIndex === 0 ? startComment : (
                mutableInnerComments[oldIndex - 1]
              )
            const end =
              oldIndex === mutableInnerComments.length ?
                endComment
              : mutableInnerComments[oldIndex]
            if (!start || !end) {
              throw new Error('Missing inner comment')
            }
            let node: Node | null
            for (
              node = start.nextSibling;
              node && node !== end;
              node = node.nextSibling
            ) {
              mutableNodes.push(node)
            }
            if (node !== end) {
              throw new Error('End comment not found')
            }
          } else {
            for await (const node of fragment.add(ctx)) {
              mutableNodes.push(node)
            }
          }
          mutableItemNodes.push(mutableNodes)
        }

        while (mutableInnerComments.length > items.length - 1) {
          mutableInnerComments.pop()
        }
        while (mutableInnerComments.length < items.length - 1) {
          mutableInnerComments.push(ctx.convey.document.createComment(''))
        }

        const mutableInnerNodes: Node[] = []
        for (let i = 0; i < mutableItemNodes.length; i++) {
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
          mutableInnerNodes.push(...mutableItemNodes[i]!)
          if (i < mutableItemNodes.length - 1) {
            const comment = mutableInnerComments[i]
            if (!comment) {
              throw new Error('Missing inner comment')
            }
            mutableInnerNodes.push(comment)
          }
        }

        innerNodes = conveyElement.replaceBetween(
          startComment,
          endComment,
          mutableInnerNodes,
        )
      }

      previousResults = newResults
    }, this.items)

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
    if (this.mutableFragments) {
      for (const [, fragment] of this.mutableFragments) {
        await fragment.remove()
      }
    }

    if (this.mutableSub !== null) {
      compute.unsubscribe(this.mutableSub)
    }
  }
}

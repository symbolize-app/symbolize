import type * as markupContext from '@/context.ts'
import * as markupElement from '@/element.ts'
import * as markupFragment from '@/fragment.ts'
import * as markupMarker from '@/marker.ts'
import * as compute from '@symbolize/lib-compute'
import type * as styling from '@symbolize/lib-styling'

export function each<CustomContext, Key, Value>(
  transform: (
    value: compute.Computation<Value>,
  ) => markupFragment.FragmentOpt<CustomContext>,
  key: (value: Value, index: number) => Key,
  items: compute.NodeOpt<readonly Value[]>,
): markupFragment.Fragment<CustomContext> {
  return new Each(transform, key, items)
}

class Each<Key, Value, CustomContext = unknown>
  implements markupFragment.Fragment<CustomContext>
{
  readonly [markupMarker.fragmentMarker]: null = null
  private mutableEndComment: Comment | null = null
  private mutableFragments:
    | [number | null, markupFragment.Fragment<CustomContext>][]
    | null = null
  private mutableInnerNodes: readonly Node[] | null = null
  private mutableStartComment: Comment | null = null
  private mutableSub: compute.Computation<void> | null = null

  constructor(
    private readonly transform: (
      value: compute.Computation<Value>,
    ) => markupFragment.FragmentOpt<CustomContext>,
    private readonly key: (value: Value, index: number) => Key,
    private readonly items: compute.NodeOpt<readonly Value[]>,
  ) {}

  async add(
    ctx: compute.Context &
      CustomContext &
      markupContext.Context &
      styling.Context,
  ): Promise<void> {
    this.mutableStartComment = ctx.markup.document.createComment('')
    const mutableInnerComments: Comment[] = []
    this.mutableEndComment = ctx.markup.document.createComment('')

    let previousResults = new Map<
      Key,
      [
        number,
        Value,
        compute.Mutation<Value>,
        markupFragment.Fragment<CustomContext>,
      ]
    >()

    this.mutableSub = await compute.effect(async (items) => {
      let rebuild = false
      const newResults: typeof previousResults = new Map()
      const mutableNewFragments: [
        number | null,
        markupFragment.Fragment<CustomContext>,
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
          markupFragment.Fragment<CustomContext>,
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
          const fragment = markupFragment.toFragment(this.transform(state))
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
              oldIndex === 0 ?
                this.mutableStartComment
              : mutableInnerComments[oldIndex - 1]
            const end =
              oldIndex === mutableInnerComments.length ?
                this.mutableEndComment
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
            await fragment.add(ctx)
            for (const node of fragment.nodes()) {
              mutableNodes.push(node)
            }
          }
          mutableItemNodes.push(mutableNodes)
        }

        if (this.mutableStartComment && this.mutableEndComment) {
          while (mutableInnerComments.length > items.length - 1) {
            mutableInnerComments.pop()
          }
          while (mutableInnerComments.length < items.length - 1) {
            mutableInnerComments.push(
              ctx.markup.document.createComment(''),
            )
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

          this.mutableInnerNodes = markupElement.replaceBetween(
            this.mutableStartComment,
            this.mutableEndComment,
            mutableInnerNodes,
          )
        }
      }

      previousResults = newResults
    }, this.items)
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
    if (this.mutableFragments) {
      for (const [, fragment] of this.mutableFragments) {
        await fragment.remove()
      }
      this.mutableFragments = null
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

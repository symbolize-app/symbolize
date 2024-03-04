import * as conveyEventMap from '@/eventMap.ts'
import * as compute from '@intertwine/lib-compute'

export interface Context {
  readonly convey: Convey
}

export interface Convey {
  readonly document: Readonly<
    Pick<
      globalThis.Document,
      | 'body'
      | 'createComment'
      | 'createElement'
      | 'createElementNS'
      | 'createTextNode'
    >
  >

  readonly scheduler: ConveyScheduler
}

export class ConveyScheduler {
  private readonly mutableQueue: (() => Promise<void>)[] = []

  async run(callback: () => Promise<void>): Promise<void> {
    return new Promise((resolve) => {
      const dispatch = async (): Promise<void> => {
        await callback()
        resolve()
        this.mutableQueue.shift()
        await this.mutableQueue[0]?.()
      }
      this.mutableQueue.push(dispatch)
      if (this.mutableQueue.length === 1) {
        void dispatch()
      }
    })
  }
}

export interface ScopedContext extends Context {
  readonly scopedConvey: ScopedConvey
}

export interface ScopedConvey {
  defer(callback: () => Promise<void> | void): void
  subscribe(sub: compute.Computation<unknown>): void
}

const fragmentMarker = Symbol('fragmentMarker')

export interface Fragment<CustomContext = unknown> {
  readonly [fragmentMarker]: null
  add(
    ctx: compute.Context & Context & CustomContext,
  ): AsyncIterableIterator<globalThis.Node>
  remove(): Promise<void>
}

export type FragmentOpt<CustomContext = unknown> =
  | compute.ComputationOpt<string>
  | Fragment<CustomContext>
  | readonly FragmentOpt<CustomContext>[]
  | null

export function toFragment<CustomContext = unknown>(
  fragment: FragmentOpt<CustomContext>,
): Fragment {
  if (typeof fragment === 'object') {
    if (fragment === null) {
      return empty()
    } else if (isReadonlyArray(fragment)) {
      return range({ content: fragment })
    } else if (fragmentMarker in fragment) {
      return fragment
    }
  }
  return text({ content: fragment })
}

function isReadonlyArray(arg: unknown): arg is readonly unknown[] {
  return Array.isArray(arg)
}

export async function scopedEffect<Args extends unknown[]>(
  ctx: ScopedContext,
  callback: (...args: Args) => Promise<void> | void,
  ...computations: compute.ComputationOptTuple<Args>
): Promise<void> {
  ctx.scopedConvey.subscribe(
    await compute.effect(callback, ...(computations as never)),
  )
}

export function scopedDefer(
  ctx: ScopedContext,
  callback: () => Promise<void> | void,
): void {
  ctx.scopedConvey.defer(callback)
}

export async function wait(ctx: Context): Promise<void> {
  return ctx.convey.scheduler.run(async () => {
    // Empty
  })
}

export function empty(): Fragment {
  return Empty.build()
}

class Empty implements Fragment {
  private static mutableEmpty: Empty | null = null;
  readonly [fragmentMarker]: null = null

  private constructor() {}

  static build(): Empty {
    if (!Empty.mutableEmpty) {
      Empty.mutableEmpty = new Empty()
    }
    return Empty.mutableEmpty
  }

  async *add(): AsyncIterableIterator<Node> {
    // Empty
  }

  async remove(): Promise<void> {
    // Empty
  }
}

export function range<CustomContext = unknown>(attrs: {
  readonly content: readonly FragmentOpt<CustomContext>[]
}): Fragment<CustomContext> {
  return new Range(attrs.content)
}

class Range<CustomContext = unknown> implements Fragment<CustomContext> {
  readonly [fragmentMarker]: null = null
  private mutableFragments: Fragment<CustomContext>[] | null = null

  constructor(
    private readonly content: readonly FragmentOpt<CustomContext>[],
  ) {}

  async *add(
    ctx: compute.Context & Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    this.mutableFragments = this.content.map(toFragment)
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

export function text(attrs: {
  readonly content: compute.ComputationOpt<string>
}): Fragment {
  return new Text(attrs.content)
}

class Text implements Fragment {
  readonly [fragmentMarker]: null = null
  private mutableEffect: compute.Computation<void> | null = null

  constructor(private readonly content: compute.ComputationOpt<string>) {}

  async *add(ctx: compute.Context & Context): AsyncIterableIterator<Node> {
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

export function defineCustom<
  CustomContext = unknown,
  Attrs extends object = Record<PropertyKey, never>,
>(
  build: (
    ctx: compute.Context & CustomContext & ScopedContext,
    attrs: Attrs,
  ) => FragmentOpt<CustomContext> | Promise<FragmentOpt<CustomContext>>,
): (attrs: Attrs) => Fragment<CustomContext> {
  return (attrs) => {
    return new Custom(build, attrs)
  }
}

class Custom<
    CustomContext = unknown,
    Attrs extends object = Record<PropertyKey, never>,
  >
  implements Fragment<CustomContext>, ScopedConvey
{
  readonly [fragmentMarker]: null = null
  private readonly mutableDeferred: (() => Promise<void> | void)[] = []
  private mutableFragment: Fragment<CustomContext> | null = null
  private readonly mutableSubs: compute.Computation<unknown>[] = []

  constructor(
    private readonly build: (
      ctx: compute.Context & CustomContext & ScopedContext,
      attrs: Attrs,
    ) => FragmentOpt<CustomContext> | Promise<FragmentOpt<CustomContext>>,
    private readonly attrs: Attrs,
  ) {}

  async *add(
    baseCtx: compute.Context & Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    const ctx: compute.Context & CustomContext & ScopedContext = {
      ...baseCtx,
      scopedConvey: this,
    }
    this.mutableFragment = toFragment(await this.build(ctx, this.attrs))
    for await (const node of this.mutableFragment.add(baseCtx)) {
      yield node
    }
  }

  defer(callback: () => Promise<void> | void): void {
    this.mutableDeferred.unshift(callback)
  }

  async remove(): Promise<void> {
    if (this.mutableFragment) {
      await this.mutableFragment.remove()
    }
    for (const callback of this.mutableDeferred) {
      await callback()
    }
    for (const sub of this.mutableSubs) {
      compute.unsubscribe(sub)
    }
  }

  subscribe(sub: compute.Computation<unknown>): void {
    this.mutableSubs.push(sub)
  }
}

type WritableElementAttrs<BaseElement extends Element> = {
  readonly [Key in Exclude<
    keyof BaseElement,
    keyof conveyEventMap.CustomEventMap | 'content'
  > as (<T>() => T extends Pick<BaseElement, Key> ? 1 : 0) extends (
    <T>() => T extends Readonly<Pick<BaseElement, Key>> ? 1 : 0
  ) ?
    never
  : BaseElement[Key] extends boolean | number | string | null ? Key
  : never]?: compute.ComputationOpt<BaseElement[Key]>
}

type ListenerElementAttrs<BaseElement extends Element> = {
  readonly [Key in keyof conveyEventMap.CustomEventMap as conveyEventMap.CustomEventMap[Key] extends (
    keyof conveyEventMap.EventMap<BaseElement>
  ) ?
    Key
  : never]?: conveyEventMap.CustomEventMap[Key] extends (
    keyof conveyEventMap.EventMap<BaseElement>
  ) ?
    (
      event: conveyEventMap.EventMap<BaseElement>[conveyEventMap.CustomEventMap[Key]],
    ) => Promise<void> | void
  : never
}

type ElementAttrs<
  CustomContext,
  BaseElement extends Element,
> = ListenerElementAttrs<BaseElement> &
  WritableElementAttrs<BaseElement> & {
    readonly content?: FragmentOpt<CustomContext>
  }

export const html = new Proxy(
  {},
  {
    get(
      _mutableTarget: object,
      property: string,
      _receiver: unknown,
    ): unknown {
      return (attrs: object) => new ElementFragment(property, attrs)
    },
  },
) as unknown as {
  readonly [Key in keyof HTMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: ElementAttrs<CustomContext, HTMLElementTagNameMap[Key]>,
  ) => Fragment<CustomContext>
}

class ElementFragment<BaseElement extends Element, CustomContext = unknown>
  implements Fragment<CustomContext>
{
  readonly [fragmentMarker]: null = null
  private mutableFragment: Fragment<CustomContext> | null = null
  private readonly mutableSubs: compute.Computation<unknown>[] = []

  constructor(
    private readonly tag: string,
    private readonly attrs: ElementAttrs<CustomContext, BaseElement>,
  ) {}

  async *add(
    ctx: compute.Context & Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    const element = ctx.convey.document.createElement(
      this.tag,
    ) as unknown as BaseElement

    const mutablePromises: Promise<void>[] = []

    for (const [key, value] of Object.entries(this.attrs)) {
      if (key === 'content') {
        mutablePromises.push(
          (async () => {
            this.mutableFragment = toFragment(value)
            for await (const node of this.mutableFragment.add(ctx)) {
              element.append(node)
            }
          })(),
        )
      } else if (key in conveyEventMap.customEventMap) {
        element.addEventListener(
          conveyEventMap.customEventMap[
            key as keyof conveyEventMap.CustomEventMap
          ],
          (event) => {
            void (async () => {
              return ctx.convey.scheduler.run(async () => {
                return compute.txn(ctx, async () => {
                  return (
                    value as unknown as (
                      event: Readonly<Event>,
                    ) => Promise<void> | void
                  )(event)
                })
              })
            })()
          },
        )
      } else {
        mutablePromises.push(
          (async () => {
            this.mutableSubs.push(
              await compute.effect((value) => {
                // eslint-disable-next-line functional/immutable-data -- required mutation
                ;(element as unknown as Record<string, unknown>)[key] =
                  value
              }, value as compute.ComputationOpt<unknown>),
            )
          })(),
        )
      }
    }

    await Promise.all(mutablePromises)

    yield element
  }

  async remove(): Promise<void> {
    if (this.mutableFragment) {
      await this.mutableFragment.remove()
    }
    for (const sub of this.mutableSubs) {
      compute.unsubscribe(sub)
    }
  }
}

type Falsy = '' | 0 | 0n | false | null | undefined

export function if_<CustomContext, Value>(
  ifBranch: (
    value: compute.Computation<Exclude<Value, Falsy>>,
  ) => FragmentOpt<CustomContext>,
  elseBranch: () => FragmentOpt<CustomContext>,
  condition: compute.ComputationOpt<Value>,
): Fragment<CustomContext> {
  return new If_(ifBranch, elseBranch, condition)
}

class If_<Value, CustomContext = unknown>
  implements Fragment<CustomContext>
{
  readonly [fragmentMarker]: null = null
  private mutableFragment: Fragment<CustomContext> | null = null
  private mutableSub: compute.Computation<void> | null = null

  constructor(
    private readonly ifBranch: (
      value: compute.Computation<Exclude<Value, Falsy>>,
    ) => FragmentOpt<CustomContext>,
    private readonly elseBranch: () => FragmentOpt<CustomContext>,
    private readonly condition: compute.ComputationOpt<Value>,
  ) {}

  async *add(
    ctx: compute.Context & Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    const startComment = ctx.convey.document.createComment('')
    const endComment = ctx.convey.document.createComment('')
    let innerNodes = null as readonly Node[] | null

    let ifResult:
      | [compute.Mutation<Exclude<Value, Falsy>>, Fragment<CustomContext>]
      | null = null
    let elseFragment: Fragment<CustomContext> | null = null

    this.mutableSub = await compute.effect(async (value) => {
      if (value) {
        const truthyValue = value as Exclude<Value, Falsy>
        if (elseFragment) {
          await elseFragment.remove()
          elseFragment = null
        }
        if (!ifResult) {
          const ifState = compute.state(truthyValue)
          const ifFragment = toFragment(this.ifBranch(ifState))
          ifResult = [ifState, ifFragment]
          const mutableIfNodes: Node[] = []
          for await (const node of ifFragment.add(ctx)) {
            mutableIfNodes.push(node)
          }
          innerNodes = replaceBetween(
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
          elseFragment = toFragment(this.elseBranch())
          const mutableElseNodes: Node[] = []
          for await (const node of elseFragment.add(ctx)) {
            mutableElseNodes.push(node)
          }
          innerNodes = replaceBetween(
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

export function each<CustomContext, Key, Value>(
  transform: (
    value: compute.Computation<Value>,
  ) => FragmentOpt<CustomContext>,
  key: (value: Value, index: number) => Key,
  items: compute.ComputationOpt<readonly Value[]>,
): Fragment<CustomContext> {
  return new Each(transform, key, items)
}

class Each<Key, Value, CustomContext = unknown>
  implements Fragment<CustomContext>
{
  readonly [fragmentMarker]: null = null
  private mutableFragments:
    | [number | null, Fragment<CustomContext>][]
    | null = null
  private mutableSub: compute.Computation<void> | null = null

  constructor(
    private readonly transform: (
      value: compute.Computation<Value>,
    ) => FragmentOpt<CustomContext>,
    private readonly key: (value: Value, index: number) => Key,
    private readonly items: compute.ComputationOpt<readonly Value[]>,
  ) {}

  async *add(
    ctx: compute.Context & Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    const startComment = ctx.convey.document.createComment('')
    const mutableInnerComments: Comment[] = []
    const endComment = ctx.convey.document.createComment('')
    let innerNodes = null as readonly Node[] | null

    let previousResults = new Map<
      Key,
      [number, Value, compute.Mutation<Value>, Fragment<CustomContext>]
    >()

    this.mutableSub = await compute.effect(async (items) => {
      let rebuild = false
      const newResults: typeof previousResults = new Map()
      const mutableNewFragments: [
        number | null,
        Fragment<CustomContext>,
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
          Fragment<CustomContext>,
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
          const fragment = toFragment(this.transform(state))
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

        innerNodes = replaceBetween(
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

function replaceBetween(
  startNode: Readonly<Node>,
  endNode: Readonly<Node>,
  innerNodes: readonly Node[],
): readonly Node[] | null {
  if (startNode.parentElement !== endNode.parentElement) {
    throw new Error("Can't replace with different parents")
  }

  const parentElement = startNode.parentElement
  if (!parentElement) {
    return innerNodes
  }

  const mutableNewNodes: Node[] = []
  let inside = false
  for (const oldNode of parentElement.childNodes) {
    if (oldNode === startNode) {
      mutableNewNodes.push(startNode)
      mutableNewNodes.push(...innerNodes)
      inside = true
    } else if (oldNode === endNode) {
      if (!inside) {
        throw new Error('End before start')
      }
      mutableNewNodes.push(endNode)
      inside = false
    } else if (!inside) {
      mutableNewNodes.push(oldNode)
    }
  }

  parentElement.replaceChildren(...mutableNewNodes)
  return null
}

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

export function div<CustomContext = unknown>(attrs: {
  readonly content?: FragmentOpt<CustomContext>
  readonly id?: compute.ComputationOpt<string>
  onclick?(this: void, event: Readonly<MouseEvent>): Promise<void> | void
}): Fragment<CustomContext> {
  return new Div(attrs)
}

class Div<CustomContext = unknown> implements Fragment<CustomContext> {
  readonly [fragmentMarker]: null = null
  private mutableFragment: Fragment<CustomContext> | null = null
  private readonly mutableSubs: compute.Computation<unknown>[] = []

  constructor(
    private readonly attrs: {
      readonly content?: FragmentOpt<CustomContext>
      readonly id?: compute.ComputationOpt<string>
      onclick?(
        this: void,
        event: Readonly<MouseEvent>,
      ): Promise<void> | void
    },
  ) {}

  async *add(
    ctx: compute.Context & Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    const mutableElement = ctx.convey.document.createElement('div')

    if (this.attrs.content) {
      this.mutableFragment = toFragment(this.attrs.content)
      for await (const node of this.mutableFragment.add(ctx)) {
        mutableElement.append(node)
      }
    }

    if (this.attrs.id) {
      this.mutableSubs.push(
        await compute.effect((id) => {
          mutableElement.id = id
        }, this.attrs.id),
      )
    }

    if (this.attrs.onclick) {
      const onclick = this.attrs.onclick
      mutableElement.addEventListener('click', (event) => {
        void (async () => {
          return ctx.convey.scheduler.run(async () => {
            return compute.txn(ctx, async () => {
              return onclick(event)
            })
          })
        })()
      })
    }

    yield mutableElement
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
          replaceBetween(startComment, endComment, mutableIfNodes)
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
          replaceBetween(startComment, endComment, mutableElseNodes)
          this.mutableFragment = elseFragment
        }
      }
    }, this.condition)

    if (!this.mutableFragment) {
      throw new Error('Internal If_ error, no fragment set')
    }

    yield startComment
    for await (const node of this.mutableFragment.add(ctx)) {
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

function replaceBetween(
  startNode: Readonly<Node>,
  endNode: Readonly<Node>,
  innerNodes: readonly Node[],
): void {
  if (startNode.parentElement !== endNode.parentElement) {
    throw new Error("Can't replace with different parents")
  }

  const parentElement = startNode.parentElement
  if (!parentElement) {
    return
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
}

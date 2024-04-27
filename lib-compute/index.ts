export interface Context {
  readonly compute: Compute
}

export class Compute {
  private mutableEpoch: Epoch = new Epoch(0)
  private readonly mutableTransactions: [
    commitImpl: CommitImpl<unknown>,
    newValue: unknown,
  ][][] = []

  get inProgressEpoch(): Epoch | null {
    return this.mutableEpoch.inProgress ? this.mutableEpoch : null
  }

  beginTransaction(): void {
    this.mutableTransactions.push([])
  }

  async commitTransaction(): Promise<void> {
    if (!this.mutableTransactions.length) {
      throw new Error('Cannot commit without transaction')
    }
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
    const transaction = this.mutableTransactions.pop()!
    if (!this.mutableTransactions.length) {
      let wait = false
      if (!this.mutableEpoch.inProgress) {
        this.mutableEpoch = new Epoch(this.mutableEpoch.id + 1)
        wait = true
      }
      for (const [commitImpl, newValue] of transaction) {
        commitImpl.commit(this.mutableEpoch, newValue)
      }
      for (const [commitImpl] of transaction) {
        await commitImpl.update(this.mutableEpoch)
      }
      if (wait) {
        await this.mutableEpoch.wait()
      }
    } else {
      const mutableParentTransaction =
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
        this.mutableTransactions[this.mutableTransactions.length - 1]!
      mutableParentTransaction.push(...transaction)
    }
  }

  rollbackTransaction(): void {
    if (!this.mutableTransactions.length) {
      throw new Error('Cannot rollback without transaction')
    }
    this.mutableTransactions.pop()
  }

  setState(mutableNodeImpl: CommitImpl<unknown>, newValue: unknown): void {
    if (!this.mutableTransactions.length) {
      throw new Error('Cannot set state without transaction')
    }
    const mutableTransaction =
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
      this.mutableTransactions[this.mutableTransactions.length - 1]!
    mutableTransaction.push([mutableNodeImpl, newValue])
  }
}

class Epoch {
  readonly mutablePromises = new Set<Promise<unknown>>()

  constructor(readonly id: number) {}

  get inProgress(): boolean {
    return !!this.mutablePromises.size
  }

  add(promise: Promise<unknown>): void {
    this.mutablePromises.add(promise)
  }

  async wait(): Promise<void> {
    while (this.mutablePromises.size) {
      const promise = this.mutablePromises.keys().next()
        .value as Promise<unknown>
      await promise
      this.mutablePromises.delete(promise)
    }
  }
}

const nodeImpl = Symbol('nodeImpl')

export interface Node<
  Value,
  Mutable extends boolean = false,
  State extends boolean = false,
> {
  readonly [nodeImpl]: NodeImpl<Value, Mutable, State>
}

type NodeImpl<Value, Mutable extends boolean, State extends boolean> =
  Mutable extends false ? ComputationImpl<Value>
  : State extends false ? MutationImpl<Value>
  : CommitImpl<Value>

interface ComputationImpl<Value> {
  readonly links: Links
  update(newEpoch: Epoch): Promise<Value>
}

interface MutationImpl<Value> extends ComputationImpl<Value> {
  set(ctx: Context, newValue: Value): Promise<void>
}

interface CommitImpl<Value> extends MutationImpl<Value> {
  commit(newEpoch: Epoch, newValue: Value): void
}

export type Computation<Value> = Node<Value> & {
  readonly [Key in keyof Value & (number | string)]: Computation<
    Value[Key]
  >
}
export type Mutation<Value> = Node<Value, true> & {
  readonly [Key in keyof Value & (number | string)]: Mutation<Value[Key]>
}

export type NodeOpt<Value> = Node<Value> | Value

export type NodeValueTuple<Tuple extends readonly NodeOpt<unknown>[]> = {
  readonly [I in keyof Tuple]: Tuple[I] extends NodeOpt<infer Value> ?
    Value
  : never
} & {
  readonly length: Tuple['length']
}

class Links {
  private readonly mutableSubs: WeakRef<Computation<unknown>>[] = []
  private readonly mutableSubscribeCount: [number] = [0]

  constructor(readonly deps: readonly Computation<unknown>[]) {}

  static subscribe(sub: Computation<unknown>): void {
    const subLinks = sub[nodeImpl].links
    subLinks.mutableSubscribeCount[0]++
    if (subLinks.mutableSubscribeCount[0] === 1) {
      for (const dep of subLinks.deps) {
        const depLinks = dep[nodeImpl].links
        depLinks.mutableSubs.push(new WeakRef(sub))
        Links.subscribe(dep)
      }
    }
  }

  static unsubscribe(sub: Computation<unknown>): void {
    const subLinks = sub[nodeImpl].links
    subLinks.mutableSubscribeCount[0]--
    if (subLinks.mutableSubscribeCount[0] === 0) {
      for (const dep of subLinks.deps) {
        const depLinks = dep[nodeImpl].links
        for (let i = 0; i < depLinks.mutableSubs.length; i++) {
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
          const otherSub = dep[nodeImpl].links.mutableSubs[i]!
          if (otherSub.deref() === sub) {
            depLinks.mutableSubs.splice(i, 1)
            Links.unsubscribe(dep)
            break
          }
        }
      }
    }
  }

  updateSubs(newEpoch: Epoch): void {
    let i = 0
    while (i < this.mutableSubs.length) {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
      const sub = this.mutableSubs[i]!.deref()
      if (!sub) {
        this.mutableSubs.splice(i, 1)
      } else {
        newEpoch.add(sub[nodeImpl].update(newEpoch))
        i++
      }
    }
  }
}

class NodeProxy
  implements ProxyHandler<NodeImpl<unknown, boolean, boolean>>
{
  private static mutableInstance: NodeProxy | null = null

  private constructor() {}

  static build<Value, Mutable extends boolean, State extends boolean>(
    mutableImpl: NodeImpl<Value, Mutable, State>,
  ): unknown {
    if (!NodeProxy.mutableInstance) {
      NodeProxy.mutableInstance = new NodeProxy()
    }
    return new Proxy(
      mutableImpl,
      NodeProxy.mutableInstance as ProxyHandler<object>,
    )
  }

  get(
    mutableTarget: NodeImpl<unknown, boolean, boolean>,
    property: PropertyKey,
    receiver: unknown,
  ): unknown {
    if (property === nodeImpl) {
      return mutableTarget
    } else if ('set' in mutableTarget) {
      return NodeProxy.build<unknown, true, false>(
        new DerivedImpl(
          new MapImpl(
            async (depValues) => {
              return Promise.resolve(
                (depValues as [Record<string | symbol, unknown>])[0][
                  property
                ],
              )
            },
            new Links([receiver as Mutation<unknown>]),
          ),
          async (ctx, newValue, depValues) => {
            if (Array.isArray(depValues[0])) {
              await set(ctx, receiver as Mutation<unknown>, [
                ...(depValues[0] as unknown[]).slice(
                  0,
                  property as number,
                ),
                newValue,
                ...(depValues[0] as unknown[]).slice(
                  (property as number) + 1,
                ),
              ])
            } else {
              await set(ctx, receiver as Mutation<unknown>, {
                ...(depValues[0] as Record<string | symbol, unknown>),
                [property]: newValue,
              })
            }
          },
        ),
      )
    } else {
      return NodeProxy.build<unknown, false, false>(
        new MapImpl(
          async (depValues) => {
            return Promise.resolve(
              (depValues as [Record<PropertyKey, unknown>])[0][property],
            )
          },
          new Links([receiver as Computation<unknown>]),
        ),
      )
    }
  }

  has(
    _mutableTarget: NodeImpl<unknown, boolean, boolean>,
    property: PropertyKey,
  ): boolean {
    return property === nodeImpl
  }
}

export async function set<Value, NewValue extends Value>(
  ctx: Context,
  mutation: Mutation<Value>,
  newValue: NewValue,
): Promise<void> {
  await mutation[nodeImpl].set(ctx, newValue)
}

export async function txn<T>(
  ctx: Context,
  callback: () => Promise<T>,
): Promise<T> {
  let result: T
  try {
    ctx.compute.beginTransaction()
    result = await callback()
  } catch (error) {
    ctx.compute.rollbackTransaction()
    throw error
  }
  await ctx.compute.commitTransaction()
  return result
}

export async function value<Value>(
  computation: NodeOpt<Value>,
): Promise<Value> {
  const epoch = new Epoch(0)
  const result = toComputation(computation)[nodeImpl].update(epoch)
  await epoch.wait()
  return result
}

export function handler<
  Result,
  EventArg,
  OtherArgs extends NodeValueTuple<NodeOptArgs>,
  NodeOptArgs extends NodeOpt<unknown>[],
>(
  callback: (
    event: EventArg,
    ...otherArgs: OtherArgs
  ) => Promise<Result> | Result,
  ...computations: NodeOptArgs
): (event: EventArg) => Promise<Result> {
  return async (event) => {
    const computationValues = (await Promise.all(
      computations.map(value),
    )) as unknown as OtherArgs
    return callback(event, ...computationValues)
  }
}

export function toComputation<Value>(
  value: NodeOpt<Value>,
): Computation<Value> {
  return typeof value === 'object' && value !== null && nodeImpl in value ?
      (value as Computation<Value>)
    : pure(value)
}

export function pure<Value>(value: Value): Computation<Value> {
  return NodeProxy.build(new PureImpl(value)) as Computation<Value>
}

export function unsubscribe(computation: Computation<unknown>): void {
  Links.unsubscribe(computation)
}

class PureImpl<Value> implements ComputationImpl<Value> {
  readonly links: Links = new Links([])

  constructor(private readonly value: Value) {}

  async update(_newEpoch: Epoch): Promise<Value> {
    return Promise.resolve(this.value)
  }
}

export function state<Value>(init: Value): Mutation<Value> {
  return NodeProxy.build<Value, true, true>(
    new StateImpl(init),
  ) as Mutation<Value>
}

class StateImpl<Value> implements CommitImpl<Value> {
  readonly links: Links = new Links([])

  private mutableEpoch: Epoch = new Epoch(0)
  private mutableUpdated: boolean = true
  private mutableValue: Value

  constructor(value: Value) {
    this.mutableValue = value
  }

  commit(newEpoch: Epoch, newValue: Value): void {
    if (this.mutableValue !== newValue) {
      this.mutableEpoch = newEpoch
      this.mutableValue = newValue
      this.mutableUpdated = false
    }
  }

  // eslint-disable-next-line @typescript-eslint/require-await -- override
  async set(ctx: Context, newValue: Value): Promise<void> {
    ctx.compute.setState(this, newValue)
  }

  async update(newEpoch: Epoch): Promise<Value> {
    if (newEpoch.id >= this.mutableEpoch.id && !this.mutableUpdated) {
      this.links.updateSubs(newEpoch)
      this.mutableUpdated = true
    }
    return Promise.resolve(this.mutableValue)
  }
}

export function map<
  Args extends NodeValueTuple<NodeOptArgs>,
  NodeOptArgs extends NodeOpt<unknown>[],
  Result,
>(
  transform: (...args: Args) => Promise<Result> | Result,
  ...computations: NodeOptArgs
): Computation<Result> {
  return NodeProxy.build(
    new MapImpl(
      async (args) => transform(...(args as Args)),
      new Links(computations.map(toComputation)),
    ),
  ) as Computation<Result>
}

type MapImplIteration<Value> = readonly [
  epoch: Epoch,
  depValues: readonly unknown[],
  value: Value,
]

class MapImpl<Value> implements ComputationImpl<Value> {
  mutableUpdateIteration: Promise<MapImplIteration<Value>> | null = null

  constructor(
    private readonly updateImpl: (
      depValues: readonly unknown[],
    ) => Promise<Value>,
    readonly links: Links,
  ) {}

  async update(newEpoch: Epoch): Promise<Value> {
    let previousIteration: MapImplIteration<Value> | null
    while (true) {
      const originalIterationPromise = this.mutableUpdateIteration
      const iteration = await this.mutableUpdateIteration
      if (this.mutableUpdateIteration === originalIterationPromise) {
        previousIteration = iteration
        break
      }
    }
    if (previousIteration && previousIteration[0].id >= newEpoch.id) {
      return previousIteration[2]
    } else {
      this.mutableUpdateIteration = this.runNewIteration(
        newEpoch,
        previousIteration,
      )
      const newIteration = await this.mutableUpdateIteration
      return newIteration[2]
    }
  }

  private async runNewIteration(
    newEpoch: Epoch,
    previousIteration: MapImplIteration<Value> | null,
  ): Promise<MapImplIteration<Value>> {
    const depValues = await Promise.all(
      this.links.deps.map(async (dep) => dep[nodeImpl].update(newEpoch)),
    )
    let value: Value
    if (
      !previousIteration ||
      depValues.some((depValue, i) => depValue !== previousIteration[1][i])
    ) {
      value = await this.updateImpl(depValues)
      if (previousIteration && value !== previousIteration[2]) {
        this.links.updateSubs(newEpoch)
      }
    } else {
      value = previousIteration[2]
    }
    return [newEpoch, depValues, value]
  }
}

export function derived<
  Result,
  Args extends NodeValueTuple<NodeOptArgs>,
  NodeOptArgs extends NodeOpt<unknown>[],
>(
  get: (...args: Args) => Promise<Result> | Result,
  set: (newValue: Result, ...args: Args) => Promise<void>,
  ...computations: NodeOptArgs
): Mutation<Result> {
  return NodeProxy.build<Result, true, false>(
    new DerivedImpl(
      new MapImpl(
        async (depValues) => {
          return get(...(depValues as Args))
        },
        new Links(computations.map(toComputation)),
      ),
      async (_ctx, newValue, depValues) => {
        await set(newValue, ...(depValues as Args))
      },
    ),
  ) as Mutation<Result>
}

class DerivedImpl<Value> implements ComputationImpl<Value> {
  constructor(
    private readonly mapImpl: ComputationImpl<Value>,
    private readonly setImpl: (
      ctx: Context,
      newValue: Value,
      depValues: readonly unknown[],
    ) => Promise<void>,
  ) {}

  get links(): Links {
    return this.mapImpl.links
  }

  async set(ctx: Context, newValue: Value): Promise<void> {
    const epoch = ctx.compute.inProgressEpoch
    const computationValues =
      epoch ?
        await Promise.all(
          this.links.deps.map(async (dep) => dep[nodeImpl].update(epoch)),
        )
      : await Promise.all(this.links.deps.map(value))
    await this.setImpl(ctx, newValue, computationValues)
  }

  async update(newEpoch: Epoch): Promise<Value> {
    return this.mapImpl.update(newEpoch)
  }
}

export async function effect<
  Args extends NodeValueTuple<NodeOptArgs>,
  NodeOptArgs extends NodeOpt<unknown>[],
>(
  callback: (...args: Args) => Promise<void> | void,
  ...computations: NodeOptArgs
): Promise<Computation<void>> {
  const node = NodeProxy.build(
    new EffectImpl(
      async (depValues) => {
        await callback(...(depValues as Args))
      },
      new Links(computations.map(toComputation)),
    ),
  ) as Computation<void>
  const epoch = new Epoch(0)
  await node[nodeImpl].update(epoch)
  await epoch.wait()
  Links.subscribe(node)
  return node
}

type EffectImplIteration = readonly [
  epoch: Epoch,
  depValues: readonly unknown[],
]

class EffectImpl implements ComputationImpl<void> {
  mutableUpdateIteration: Promise<EffectImplIteration> | null = null

  constructor(
    private readonly updateImpl: (
      depValues: readonly unknown[],
    ) => Promise<void>,
    readonly links: Links,
  ) {}

  async update(newEpoch: Epoch): Promise<void> {
    let previousIteration: EffectImplIteration | null
    while (true) {
      const originalIterationPromise = this.mutableUpdateIteration
      const iteration = await this.mutableUpdateIteration
      if (this.mutableUpdateIteration === originalIterationPromise) {
        previousIteration = iteration
        break
      }
    }
    if (previousIteration && previousIteration[0].id >= newEpoch.id) {
      // Already done
    } else {
      this.mutableUpdateIteration = this.runNewIteration(
        newEpoch,
        previousIteration,
      )
      await this.mutableUpdateIteration
    }
  }

  private async runNewIteration(
    newEpoch: Epoch,
    previousIteration: EffectImplIteration | null,
  ): Promise<EffectImplIteration> {
    const depValues = await Promise.all(
      this.links.deps.map(async (dep) => dep[nodeImpl].update(newEpoch)),
    )
    if (
      !previousIteration ||
      depValues.some((depValue, i) => depValue !== previousIteration[1][i])
    ) {
      await this.updateImpl(depValues)
    }
    return [newEpoch, depValues]
  }
}

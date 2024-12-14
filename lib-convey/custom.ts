import type * as conveyContext from '@/context.ts'
import * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'

export function defineCustom<
  CustomContext = unknown,
  Attrs extends object = Record<PropertyKey, never>,
>(
  build: (
    ctx: compute.Context & conveyContext.ScopedContext & CustomContext,
    attrs: Attrs,
  ) =>
    | conveyFragment.FragmentOpt<CustomContext>
    | Promise<conveyFragment.FragmentOpt<CustomContext>>,
): (attrs: Attrs) => conveyFragment.Fragment<CustomContext> {
  return (attrs) => {
    return new Custom(build, attrs)
  }
}

class Custom<
    CustomContext = unknown,
    Attrs extends object = Record<PropertyKey, never>,
  >
  implements
    conveyFragment.Fragment<CustomContext>,
    conveyContext.ScopedConvey
{
  readonly [conveyMarker.fragmentMarker]: null = null
  private readonly mutableDeferred: (() => Promise<void> | void)[] = []
  private mutableFragment: conveyFragment.Fragment<CustomContext> | null =
    null
  private readonly mutableSubs: compute.Computation<unknown>[] = []

  constructor(
    private readonly build: (
      ctx: compute.Context & conveyContext.ScopedContext & CustomContext,
      attrs: Attrs,
    ) =>
      | conveyFragment.FragmentOpt<CustomContext>
      | Promise<conveyFragment.FragmentOpt<CustomContext>>,
    private readonly attrs: Attrs,
  ) {}

  async add(
    baseCtx: compute.Context &
      contrast.Context &
      conveyContext.Context &
      CustomContext,
  ): Promise<void> {
    const ctx: compute.Context &
      conveyContext.ScopedContext &
      CustomContext = {
      ...baseCtx,
      scopedConvey: this,
    }
    this.mutableFragment = conveyFragment.toFragment(
      await this.build(ctx, this.attrs),
    )
    await this.mutableFragment.add(baseCtx)
  }

  defer(callback: () => Promise<void> | void): void {
    this.mutableDeferred.unshift(callback)
  }

  *nodes(): IterableIterator<Node> {
    if (this.mutableFragment) {
      for (const node of this.mutableFragment.nodes()) {
        yield node
      }
    }
  }

  async remove(): Promise<void> {
    if (this.mutableFragment) {
      await this.mutableFragment.remove()
      this.mutableFragment = null
    }
    for (const callback of this.mutableDeferred) {
      await callback()
    }
    this.mutableDeferred.length = 0
    for (const sub of this.mutableSubs) {
      compute.unsubscribe(sub)
    }
    this.mutableSubs.length = 0
  }

  subscribe(sub: compute.Computation<unknown>): void {
    this.mutableSubs.push(sub)
  }
}

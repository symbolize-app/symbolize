import type * as conveyContext from '@/context.ts'
import * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import * as compute from '@intertwine/lib-compute'

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

  async *add(
    baseCtx: compute.Context & conveyContext.Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    const ctx: compute.Context &
      conveyContext.ScopedContext &
      CustomContext = {
      ...baseCtx,
      scopedConvey: this,
    }
    this.mutableFragment = conveyFragment.toFragment(
      await this.build(ctx, this.attrs),
    )
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

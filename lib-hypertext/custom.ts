import type * as hypertextContext from '@/context.ts'
import * as hypertextFragment from '@/fragment.ts'
import * as hypertextMarker from '@/marker.ts'
import * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'

export function defineCustom<
  CustomContext = unknown,
  Attrs extends object = Record<PropertyKey, never>,
>(
  build: (
    ctx: compute.Context & CustomContext & hypertextContext.ScopedContext,
    attrs: Attrs,
  ) =>
    | hypertextFragment.FragmentOpt<CustomContext>
    | Promise<hypertextFragment.FragmentOpt<CustomContext>>,
): (attrs: Attrs) => hypertextFragment.Fragment<CustomContext> {
  return (attrs) => {
    return new Custom(build, attrs)
  }
}

class Custom<
    CustomContext = unknown,
    Attrs extends object = Record<PropertyKey, never>,
  >
  implements
    hypertextFragment.Fragment<CustomContext>,
    hypertextContext.ScopedConvey
{
  readonly [hypertextMarker.fragmentMarker]: null = null
  private readonly mutableDeferred: (() => Promise<void> | void)[] = []
  private mutableFragment: hypertextFragment.Fragment<CustomContext> | null =
    null
  private readonly mutableSubs: compute.Computation<unknown>[] = []

  constructor(
    private readonly build: (
      ctx: compute.Context &
        CustomContext &
        hypertextContext.ScopedContext,
      attrs: Attrs,
    ) =>
      | hypertextFragment.FragmentOpt<CustomContext>
      | Promise<hypertextFragment.FragmentOpt<CustomContext>>,
    private readonly attrs: Attrs,
  ) {}

  async add(
    baseCtx: compute.Context &
      contrast.Context &
      CustomContext &
      hypertextContext.Context,
  ): Promise<void> {
    const ctx: compute.Context &
      CustomContext &
      hypertextContext.ScopedContext = {
      ...baseCtx,
      scopedConvey: this,
    }
    this.mutableFragment = hypertextFragment.toFragment(
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

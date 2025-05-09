import type * as markupContext from '@/context.ts'
import * as markupFragment from '@/fragment.ts'
import * as markupMarker from '@/marker.ts'
import * as dataflow from '@symbolize/lib-dataflow'
import type * as styling from '@symbolize/lib-styling'

export function defineCustom<
  CustomContext = unknown,
  Attrs extends object = Record<PropertyKey, never>,
>(
  build: (
    ctx: CustomContext & dataflow.Context & markupContext.ScopedContext,
    attrs: Attrs,
  ) =>
    | markupFragment.FragmentOpt<CustomContext>
    | Promise<markupFragment.FragmentOpt<CustomContext>>,
): (attrs: Attrs) => markupFragment.Fragment<CustomContext> {
  return (attrs) => {
    return new Custom(build, attrs)
  }
}

class Custom<
    CustomContext = unknown,
    Attrs extends object = Record<PropertyKey, never>,
  >
  implements
    markupFragment.Fragment<CustomContext>,
    markupContext.ScopedConvey
{
  readonly [markupMarker.fragmentMarker]: null = null
  private readonly mutableDeferred: (() => Promise<void> | void)[] = []
  private mutableFragment: markupFragment.Fragment<CustomContext> | null =
    null
  private readonly mutableSubs: dataflow.Computation<unknown>[] = []

  constructor(
    private readonly build: (
      ctx: CustomContext & dataflow.Context & markupContext.ScopedContext,
      attrs: Attrs,
    ) =>
      | markupFragment.FragmentOpt<CustomContext>
      | Promise<markupFragment.FragmentOpt<CustomContext>>,
    private readonly attrs: Attrs,
  ) {}

  async add(
    baseCtx: CustomContext &
      dataflow.Context &
      markupContext.Context &
      styling.Context,
  ): Promise<void> {
    const ctx: CustomContext &
      dataflow.Context &
      markupContext.ScopedContext = {
      ...baseCtx,
      scopedConvey: this,
    }
    this.mutableFragment = markupFragment.toFragment(
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
      dataflow.unsubscribe(sub)
    }
    this.mutableSubs.length = 0
  }

  subscribe(sub: dataflow.Computation<unknown>): void {
    this.mutableSubs.push(sub)
  }
}

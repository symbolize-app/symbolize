import type * as hypertextScheduler from '@/scheduler.ts'
import * as compute from '@symbolize/lib-compute'

export interface Context {
  readonly hypertext: Hypertext
}

export interface Hypertext {
  readonly classNames: Set<string>

  readonly document: RestrictedDocument

  readonly scheduler: hypertextScheduler.Scheduler

  readonly styleLayer: RestrictedCssLayerBlockRule
}

export type RestrictedDocument = Readonly<
  Pick<
    globalThis.Document,
    | 'body'
    | 'createComment'
    | 'createElement'
    | 'createElementNS'
    | 'createTextNode'
    | 'documentElement'
    | 'head'
  >
>

export type RestrictedCssLayerBlockRule = Pick<
  CSSLayerBlockRule,
  'insertRule'
> & { readonly cssRules: RestrictedCssRuleList }

export type RestrictedCssRuleList = Pick<CSSRuleList, 'length'> & {
  [Symbol.iterator](): IterableIterator<RestrictedCssRule>
}

export type RestrictedCssRule = Readonly<Pick<CSSRule, 'cssText'>>

export interface ScopedContext extends Context {
  readonly scopedConvey: ScopedConvey
}

export interface ScopedConvey {
  defer(callback: () => Promise<void> | void): void
  subscribe(sub: compute.Computation<unknown>): void
}

export async function scopedEffect<
  Args extends compute.NodeValueTuple<NodeOptArgs>,
  NodeOptArgs extends compute.NodeOpt<unknown>[],
>(
  ctx: ScopedContext,
  callback: (...args: Args) => Promise<void> | void,
  ...computations: NodeOptArgs
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
  return ctx.hypertext.scheduler.wait()
}

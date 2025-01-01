import type * as markupScheduler from '@/scheduler.ts'
import * as dataflow from '@symbolize/lib-dataflow'

export interface Context {
  readonly markup: Markup
}

export interface Markup {
  readonly classNames: Set<string>

  readonly document: RestrictedDocument

  readonly scheduler: markupScheduler.Scheduler

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
  subscribe(sub: dataflow.Computation<unknown>): void
}

export async function scopedEffect<
  Args extends dataflow.NodeValueTuple<NodeOptArgs>,
  NodeOptArgs extends dataflow.NodeOpt<unknown>[],
>(
  ctx: ScopedContext,
  callback: (...args: Args) => Promise<void> | void,
  ...computations: NodeOptArgs
): Promise<void> {
  ctx.scopedConvey.subscribe(
    await dataflow.effect(callback, ...(computations as never)),
  )
}

export function scopedDefer(
  ctx: ScopedContext,
  callback: () => Promise<void> | void,
): void {
  ctx.scopedConvey.defer(callback)
}

export async function wait(ctx: Context): Promise<void> {
  return ctx.markup.scheduler.wait()
}

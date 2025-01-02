import type * as stylingContext from '@/context.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'
import type * as stylingUtilityType from '@/utilityType.ts'

const selectMarker = Symbol('selectMarker')

class SelectTerm {
  constructor(
    private readonly compile_: (...args: readonly unknown[]) => string,
    private readonly args: (
      ctx: stylingContext.Context,
    ) => readonly unknown[],
  ) {}

  compile(ctx: stylingContext.Context): string {
    const internArgs = this.args(ctx)
    return ctx.styling.scopeIntern.get(this.compile_, ...internArgs)
  }

  [selectMarker](): unknown {
    return null
  }
}

export type { SelectTerm }

export const select = {
  term<Compile extends (...args: never) => string>(
    compile: Compile,
    args: (
      ctx: stylingContext.Context,
    ) => stylingUtilityType.ReadonlyParameters<Compile>,
  ): SelectTerm {
    return new SelectTerm(compile as never, args as never)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_selectors
   */
  match<Value>(
    condition: SelectTerm,
    value: stylingExpression.ExpressionOpt<Value>,
  ): stylingExpression.Expression<Value> {
    return stylingExpression.expression(compileMatch, (ctx) => [
      condition.compile(ctx),
      stylingExpression.compile(ctx, value),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_selectors/Selector_structure#compound_selector
   */
  and(value: SelectTerm, ...values: readonly SelectTerm[]): SelectTerm {
    return select.term(compileAnd, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/:not
   */
  not(value: SelectTerm, ...values: readonly SelectTerm[]): SelectTerm {
    const term = select.and(value, ...values)
    return select.term(compileNot, (ctx) => [term.compile(ctx)])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/Selector_list
   */
  or(value: SelectTerm, ...values: readonly SelectTerm[]): SelectTerm {
    return select.term(compileOr, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/:dir
   */
  dir(value: 'ltr' | 'rtl'): SelectTerm {
    return stateFunction('dir', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/:disabled
   */
  disabled(): SelectTerm {
    return state('disabled')
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/:empty
   */
  empty(): SelectTerm {
    return state('empty')
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/:hover
   */
  hover(): SelectTerm {
    return state('hover')
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/:first-child
   */
  firstChild(): SelectTerm {
    return state('first-child')
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/:lang
   */
  lang(value: string, ...values: readonly string[]): SelectTerm {
    return stateFunction('lang', value, ...values)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/:last-child
   */
  lastChild(): SelectTerm {
    return state('last-child')
  },
}

function compileMatch(
  condition: string,
  value: stylingExpressionIntern.ExpressionIntern,
): stylingExpressionIntern.ExpressionIntern {
  return stylingExpressionIntern.compileScope(
    `&:where(${condition})`,
    value,
  )
}

function compileAnd(...values: readonly string[]): string {
  return values.join('')
}

function compileNot(value: string): string {
  return `:not(${value})`
}

function compileOr(...values: readonly string[]): string {
  return `:where(${values.join(',')})`
}

function state(name: string): SelectTerm {
  return select.term(compileState, () => [name])
}

function compileState(name: string): string {
  return `:${name}`
}

function stateFunction(
  name: string,
  ...args: readonly unknown[]
): SelectTerm {
  return select.term(compileStateFunction, () => [name, ...args])
}

function compileStateFunction(
  name: string,
  ...args: readonly unknown[]
): string {
  return `:${name}(${args.join(',')})`
}

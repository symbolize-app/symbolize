import * as stylingAtom from '@/atom.ts'
import type * as stylingContext from '@/context.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'
import type * as stylingUtilityType from '@/utilityType.ts'

const supportMarker = Symbol('supportMarker')

class SupportTerm {
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

  [supportMarker](): unknown {
    return null
  }
}

export type { SupportTerm }

export const support = {
  term<Compile extends (...args: never) => string>(
    compile: Compile,
    args: (
      ctx: stylingContext.Context,
    ) => stylingUtilityType.ReadonlyParameters<Compile>,
  ): SupportTerm {
    return new SupportTerm(compile as never, args)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports
   */
  match<Value>(
    condition: SupportTerm,
    value: stylingExpression.ExpressionOpt<Value>,
  ): stylingExpression.Expression<Value> {
    return stylingExpression.expression(compileMatch, (ctx) => [
      condition.compile(ctx),
      stylingExpression.compile(ctx, value),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports#the_and_operator
   */
  and(value: SupportTerm, ...values: readonly SupportTerm[]): SupportTerm {
    return support.term(stylingExpressionIntern.compileScopeAnd, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports#the_not_operator
   */
  not(value: SupportTerm, ...values: readonly SupportTerm[]): SupportTerm {
    const term = support.and(value, ...values)
    return support.term(stylingExpressionIntern.compileScopeNot, (ctx) => [
      term.compile(ctx),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports#the_or_operator
   */
  or(value: SupportTerm, ...values: readonly SupportTerm[]): SupportTerm {
    return support.term(stylingExpressionIntern.compileScopeOr, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   *
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports#declaration_syntax
   */
  code(value: stylingAtom.AtomOpt): SupportTerm {
    const terms = [...stylingAtom.toAtomIterable(value)].map((atom) =>
      support.term(compileCode, (ctx) => {
        const atomIntern = atom.compile(ctx)
        const expressionIntern =
          stylingExpressionIntern.compileScopePureExpressionIntern(
            ctx,
            atomIntern.expressionIntern,
          )
        return [atomIntern.propertyName, expressionIntern]
      }),
    ) as [value: SupportTerm, ...values: readonly SupportTerm[]]
    return support.and(...terms)
  },
}

function compileMatch(
  condition: string,
  value: stylingExpressionIntern.ExpressionIntern,
): stylingExpressionIntern.ExpressionIntern {
  return stylingExpressionIntern.compileScope(
    `@supports ${condition}`,
    value,
  )
}

function compileCode(
  propertyName: string,
  expressionIntern: stylingExpressionIntern.PureExpressionIntern,
): string {
  return ''.concat('(', ...expressionIntern.code(propertyName), ')')
}

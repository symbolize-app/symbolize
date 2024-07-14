import * as contrastAtom from '@/atom.ts'
import type * as contrastContext from '@/context.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastUtilityType from '@/utilityType.ts'

const supportMarker = Symbol('supportMarker')

export class SupportTerm {
  constructor(
    private readonly compile_: (...args: readonly unknown[]) => string,
    private readonly args: (
      ctx: contrastContext.Context,
    ) => readonly unknown[],
  ) {}

  compile(ctx: contrastContext.Context): string {
    const internArgs = this.args(ctx)
    return ctx.contrast.scopeIntern.get(this.compile_, ...internArgs)
  }

  [supportMarker](): unknown {
    return null
  }
}

export const support = {
  term<Compile extends (...args: never) => string>(
    compile: Compile,
    args: (
      ctx: contrastContext.Context,
    ) => contrastUtilityType.ReadonlyParameters<Compile>,
  ): SupportTerm {
    return new SupportTerm(compile as never, args)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports
   */
  match<Value>(
    condition: SupportTerm,
    value: contrastExpression.ExpressionOpt<Value>,
  ): contrastExpression.Expression<Value> {
    return contrastExpression.expression(compileMatch, (ctx) => [
      condition.compile(ctx),
      contrastExpression.compile(ctx, value),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports#the_and_operator
   */
  and(value: SupportTerm, ...values: readonly SupportTerm[]): SupportTerm {
    return support.term(contrastExpressionIntern.compileScopeAnd, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports#the_not_operator
   */
  not(value: SupportTerm, ...values: readonly SupportTerm[]): SupportTerm {
    const term = support.and(value, ...values)
    return support.term(
      contrastExpressionIntern.compileScopeNot,
      (ctx) => [term.compile(ctx)],
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports#the_or_operator
   */
  or(value: SupportTerm, ...values: readonly SupportTerm[]): SupportTerm {
    return support.term(contrastExpressionIntern.compileScopeOr, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   *
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@supports#declaration_syntax
   */
  code(value: contrastAtom.AtomOpt): SupportTerm {
    const terms = [...contrastAtom.toAtomIterable(value)].map((atom) =>
      support.term(compileCode, (ctx) => {
        const atomIntern = atom.compile(ctx)
        const expressionIntern =
          contrastExpressionIntern.compileScopePureExpressionIntern(
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
  value: contrastExpressionIntern.ExpressionIntern,
): contrastExpressionIntern.ExpressionIntern {
  return contrastExpressionIntern.compileScope(
    `@supports ${condition}`,
    value,
  )
}

function compileCode(
  propertyName: string,
  expressionIntern: contrastExpressionIntern.PureExpressionIntern,
): string {
  return ''.concat('(', ...expressionIntern.code(propertyName), ')')
}

import type * as stylingContext from '@/context.ts'
import type * as stylingData from '@/data/index.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'
import type * as stylingUtilityType from '@/utilityType.ts'

const mediaMarker = Symbol('mediaMarker')

class MediaTerm {
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

  [mediaMarker](): unknown {
    return null
  }
}

export type { MediaTerm }

export const media = {
  term<Compile extends (...args: never) => string>(
    compile: Compile,
    args: (
      ctx: stylingContext.Context,
    ) => stylingUtilityType.ReadonlyParameters<Compile>,
  ): MediaTerm {
    return new MediaTerm(compile as never, args)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media
   */
  match<Value>(
    type: 'all' | 'print' | 'screen',
    condition: MediaTerm,
    value: stylingExpression.ExpressionOpt<Value>,
  ): stylingExpression.Expression<Value> {
    return stylingExpression.expression(compileMatch, (ctx) => [
      type,
      condition.compile(ctx),
      stylingExpression.compile(ctx, value),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media#logical_operators
   */
  and(value: MediaTerm, ...values: readonly MediaTerm[]): MediaTerm {
    return media.term(stylingExpressionIntern.compileScopeAnd, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media#logical_operators
   */
  not(value: MediaTerm, ...values: readonly MediaTerm[]): MediaTerm {
    const term = media.and(value, ...values)
    return media.term(stylingExpressionIntern.compileScopeNot, (ctx) => [
      term.compile(ctx),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media#logical_operators
   */
  or(value: MediaTerm, ...values: readonly MediaTerm[]): MediaTerm {
    return media.term(stylingExpressionIntern.compileScopeOr, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  anyInput: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/any-hover
     */
    hover(...value: readonly ['hover' | 'none'] | readonly []): MediaTerm {
      return mediaFeature(
        'any-hover',
        ...(value.length ? value : (['hover'] as const)),
      )
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/any-pointer
     */
    pointer(value: 'coarse' | 'fine' | 'none'): MediaTerm {
      return mediaFeature('any-pointer', value)
    },
  },

  hint: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-styling
     */
    styling(
      value: 'custom' | 'less' | 'more' | 'no-preference',
    ): MediaTerm {
      return mediaFeature('prefers-styling', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-reduced-motion
     */
    reducedMotion(
      ...value: readonly ['no-preference' | 'reduce'] | readonly []
    ): MediaTerm {
      return mediaFeature(
        'prefers-reduced-motion',
        ...(value.length ? value : (['reduce'] as const)),
      )
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme
     */
    colorScheme(value: 'dark' | 'light'): MediaTerm {
      return mediaFeature('prefers-color-scheme', value)
    },
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/hover
   */
  hover(...value: readonly ['hover' | 'none'] | readonly []): MediaTerm {
    return mediaFeature(
      'hover',
      ...(value.length ? value : (['hover'] as const)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/orientation
   */
  orientation(value: 'landscape' | 'portrait'): MediaTerm {
    return mediaFeature('orientation', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/pointer
   */
  pointer(value: 'coarse' | 'fine' | 'none'): MediaTerm {
    return mediaFeature('pointer', value)
  },

  min: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/width
     */
    w(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): MediaTerm {
      return mediaExpressionFeature('min-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/height
     */
    h(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): MediaTerm {
      return mediaExpressionFeature('min-height', value)
    },
  },

  max: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/width
     */
    w(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): MediaTerm {
      return mediaExpressionFeature('max-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/height
     */
    h(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): MediaTerm {
      return mediaExpressionFeature('max-height', value)
    },
  },
}

function compileMatch(
  type: string,
  condition: string,
  value: stylingExpressionIntern.ExpressionIntern,
): stylingExpressionIntern.ExpressionIntern {
  return stylingExpressionIntern.compileScope(
    `@media only ${type} and ${condition}`,
    value,
  )
}

function mediaFeature(featureName: string, value: string): MediaTerm {
  return media.term(compileFeature, () => [featureName, value])
}

function compileFeature(featureName: string, value: string): string {
  return `(${featureName}: ${value})`
}

function mediaExpressionFeature(
  featureName: string,
  value: stylingExpression.ExpressionOpt<unknown>,
): MediaTerm {
  return media.term(compileExpressionFeature, (ctx) => [
    featureName,
    stylingExpression.compileScopePureExpression(ctx, value),
  ])
}

function compileExpressionFeature(
  featureName: string,
  expressionIntern: stylingExpressionIntern.PureExpressionIntern,
): string {
  return `(${featureName}: ${expressionIntern.value})`
}

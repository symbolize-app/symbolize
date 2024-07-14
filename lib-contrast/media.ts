import type * as contrastContext from '@/context.ts'
import type * as contrastData from '@/data/index.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastUtilityType from '@/utilityType.ts'

const mediaMarker = Symbol('mediaMarker')

export class MediaTerm {
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

  [mediaMarker](): unknown {
    return null
  }
}

export const media = {
  term<Compile extends (...args: never) => string>(
    compile: Compile,
    args: (
      ctx: contrastContext.Context,
    ) => contrastUtilityType.ReadonlyParameters<Compile>,
  ): MediaTerm {
    return new MediaTerm(compile as never, args)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media
   */
  match<Value>(
    type: 'all' | 'print' | 'screen',
    condition: MediaTerm,
    value: contrastExpression.ExpressionOpt<Value>,
  ): contrastExpression.Expression<Value> {
    return contrastExpression.expression(compileMatch, (ctx) => [
      type,
      condition.compile(ctx),
      contrastExpression.compile(ctx, value),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media#logical_operators
   */
  and(value: MediaTerm, ...values: readonly MediaTerm[]): MediaTerm {
    return media.term(contrastExpressionIntern.compileScopeAnd, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media#logical_operators
   */
  not(value: MediaTerm, ...values: readonly MediaTerm[]): MediaTerm {
    const term = media.and(value, ...values)
    return media.term(contrastExpressionIntern.compileScopeNot, (ctx) => [
      term.compile(ctx),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media#logical_operators
   */
  or(value: MediaTerm, ...values: readonly MediaTerm[]): MediaTerm {
    return media.term(contrastExpressionIntern.compileScopeOr, (ctx) =>
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
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-contrast
     */
    contrast(
      value: 'custom' | 'less' | 'more' | 'no-preference',
    ): MediaTerm {
      return mediaFeature('prefers-contrast', value)
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
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): MediaTerm {
      return mediaExpressionFeature('min-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/height
     */
    h(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
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
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): MediaTerm {
      return mediaExpressionFeature('max-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@media/height
     */
    h(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): MediaTerm {
      return mediaExpressionFeature('max-height', value)
    },
  },
}

function compileMatch(
  type: string,
  condition: string,
  value: contrastExpressionIntern.ExpressionIntern,
): contrastExpressionIntern.ExpressionIntern {
  return contrastExpressionIntern.compileScope(
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
  value: contrastExpression.ExpressionOpt<unknown>,
): MediaTerm {
  return media.term(compileExpressionFeature, (ctx) => [
    featureName,
    contrastExpression.compileScopePureExpression(ctx, value),
  ])
}

function compileExpressionFeature(
  featureName: string,
  expressionIntern: contrastExpressionIntern.PureExpressionIntern,
): string {
  return `(${featureName}: ${expressionIntern.value})`
}

import * as stylingAtom from '@/atom.ts'
import type * as stylingContext from '@/context.ts'
import type * as stylingData from '@/data/index.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'
import type * as stylingUtilityType from '@/utilityType.ts'

const containerMarker = Symbol('containerMarker')
const containerNameMarker = Symbol('containerNameMarker')
const containerNameValueMarker = Symbol('containerNameValueMarker')

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/container-name
 */
class Container implements stylingExpression.Expression<ContainerName> {
  private readonly value = Symbol('containerName')

  compile(
    ctx: stylingContext.Context,
  ): stylingExpressionIntern.ExpressionIntern {
    return ctx.styling.expressionIntern.get(
      stylingExpressionIntern.compilePure as never,
      this.resolve(ctx),
    )
  }

  [containerNameMarker](): unknown {
    return null
  }

  resolve(ctx: stylingContext.Context): string {
    return ctx.styling.containerName.get(this.value)
  }

  [stylingExpression.expressionMarker](): unknown {
    return null
  }

  [stylingExpression.expressionValueMarker](): ContainerName | null {
    return null
  }
}

export type { Container }

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/container-name
 */
export interface ContainerName {
  [containerNameValueMarker](): unknown
}

class ContainerTerm {
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

  [containerMarker](): unknown {
    return null
  }
}

export type { ContainerTerm }

export const container = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/container-name
   */
  build(): Container {
    return new Container()
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/container-name
   */
  name(
    value: stylingExpression.ExpressionOpt<ContainerName>,
    ...values: readonly stylingExpression.ExpressionOpt<ContainerName>[]
  ): stylingAtom.Atom {
    return stylingAtom.atom(
      'container-name',
      stylingExpression.expression(
        stylingExpressionIntern.compilePureSpaceSeparator,
        (ctx) => stylingExpression.compileAllToPure(ctx, value, ...values),
      ),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/container-type
   */
  type(
    value: stylingExpression.ExpressionOpt<
      'inline-size' | 'normal' | 'size'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('container-type', value)
  },

  term<Compile extends (...args: never) => string>(
    compile: Compile,
    args: (
      ctx: stylingContext.Context,
    ) => stylingUtilityType.ReadonlyParameters<Compile>,
  ): ContainerTerm {
    return new ContainerTerm(compile as never, args)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container
   */
  match<Value>(
    name: Container | 'all',
    condition: ContainerTerm,
    value: stylingExpression.ExpressionOpt<Value>,
  ): stylingExpression.Expression<Value> {
    return stylingExpression.expression(compileMatch, (ctx) => [
      name === 'all' ? null : name.resolve(ctx),
      condition.compile(ctx),
      stylingExpression.compile(ctx, value),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#logical_keywords_in_container_queries
   */
  and(
    value: ContainerTerm,
    ...values: readonly ContainerTerm[]
  ): ContainerTerm {
    return container.term(stylingExpressionIntern.compileScopeAnd, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#logical_keywords_in_container_queries
   */
  not(
    value: ContainerTerm,
    ...values: readonly ContainerTerm[]
  ): ContainerTerm {
    const term = container.and(value, ...values)
    return container.term(
      stylingExpressionIntern.compileScopeNot,
      (ctx) => [term.compile(ctx)],
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#logical_keywords_in_container_queries
   */
  or(
    value: ContainerTerm,
    ...values: readonly ContainerTerm[]
  ): ContainerTerm {
    return container.term(stylingExpressionIntern.compileScopeOr, (ctx) =>
      [value, ...values].map((value) => value.compile(ctx)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
   */
  orientation(value: 'landscape' | 'portrait'): ContainerTerm {
    return containerFeature('orientation', value)
  },

  min: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    w(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('min-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    h(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('min-height', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    o(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('min-block-size', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    i(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('min-inline-size', value)
    },
  },

  max: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    w(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('max-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    h(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('max-height', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    o(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('max-block-size', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    i(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('max-inline-size', value)
    },
  },
}

function compileMatch(
  name: string | null,
  condition: string,
  value: stylingExpressionIntern.ExpressionIntern,
): stylingExpressionIntern.ExpressionIntern {
  return stylingExpressionIntern.compileScope(
    name ? `@container ${name} ${condition}` : `@container ${condition}`,
    value,
  )
}

function containerFeature(
  featureName: string,
  value: string,
): ContainerTerm {
  return container.term(compileFeature, () => [featureName, value])
}

function compileFeature(featureName: string, value: string): string {
  return `(${featureName}: ${value})`
}

function containerExpressionFeature(
  featureName: string,
  value: stylingExpression.ExpressionOpt<unknown>,
): ContainerTerm {
  return container.term(compileExpressionFeature, (ctx) => [
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

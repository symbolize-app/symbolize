import * as contrastAtom from '@/atom.ts'
import type * as contrastContext from '@/context.ts'
import type * as contrastData from '@/data/index.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastUtilityType from '@/utilityType.ts'

const containerMarker = Symbol('containerMarker')
const containerNameMarker = Symbol('containerNameMarker')
const containerNameValueMarker = Symbol('containerNameValueMarker')

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/container-name
 */
export class Container
  implements contrastExpression.Expression<ContainerName>
{
  private readonly value = Symbol('containerName')

  compile(
    ctx: contrastContext.Context,
  ): contrastExpressionIntern.ExpressionIntern {
    return ctx.contrast.expressionIntern.get(
      contrastExpressionIntern.compilePure as never,
      this.resolve(ctx),
    )
  }

  [containerNameMarker](): unknown {
    return null
  }

  [contrastExpression.expressionMarker](): unknown {
    return null
  }

  [contrastExpression.expressionValueMarker](): ContainerName | null {
    return null
  }

  resolve(ctx: contrastContext.Context): string {
    return ctx.contrast.containerName.get(this.value)
  }
}

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/container-name
 */
export interface ContainerName {
  [containerNameValueMarker](): unknown
}

export class ContainerTerm {
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

  [containerMarker](): unknown {
    return null
  }
}

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
    value: contrastExpression.ExpressionOpt<ContainerName>,
    ...values: readonly contrastExpression.ExpressionOpt<ContainerName>[]
  ): contrastAtom.Atom {
    return contrastAtom.atom(
      'container-name',
      contrastExpression.expression(
        contrastExpressionIntern.compilePureSpaceSeparator,
        (ctx) =>
          contrastExpression.compileAllToPure(ctx, value, ...values),
      ),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/container-type
   */
  type(
    value: contrastExpression.ExpressionOpt<
      'inline-size' | 'normal' | 'size'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('container-type', value)
  },

  term<Compile extends (...args: never) => string>(
    compile: Compile,
    args: (
      ctx: contrastContext.Context,
    ) => contrastUtilityType.ReadonlyParameters<Compile>,
  ): ContainerTerm {
    return new ContainerTerm(compile as never, args)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container
   */
  match<Value>(
    name: Container | 'all',
    condition: ContainerTerm,
    value: contrastExpression.ExpressionOpt<Value>,
  ): contrastExpression.Expression<Value> {
    return contrastExpression.expression(compileMatch, (ctx) => [
      name === 'all' ? null : name.resolve(ctx),
      condition.compile(ctx),
      contrastExpression.compile(ctx, value),
    ])
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#logical_keywords_in_container_queries
   */
  and(
    value: ContainerTerm,
    ...values: readonly ContainerTerm[]
  ): ContainerTerm {
    return container.term(
      contrastExpressionIntern.compileScopeAnd,
      (ctx) => [value, ...values].map((value) => value.compile(ctx)),
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
      contrastExpressionIntern.compileScopeNot,
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
    return container.term(contrastExpressionIntern.compileScopeOr, (ctx) =>
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
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('min-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    h(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('min-height', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    o(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('min-block-size', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    i(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
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
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('max-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    h(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('max-height', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    o(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('max-block-size', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/@container#descriptors
     */
    i(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): ContainerTerm {
      return containerExpressionFeature('max-inline-size', value)
    },
  },
}

function compileMatch(
  name: string | null,
  condition: string,
  value: contrastExpressionIntern.ExpressionIntern,
): contrastExpressionIntern.ExpressionIntern {
  return contrastExpressionIntern.compileScope(
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
  value: contrastExpression.ExpressionOpt<unknown>,
): ContainerTerm {
  return container.term(compileExpressionFeature, (ctx) => [
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

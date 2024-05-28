import * as contrastAtomIntern from '@/atomIntern.ts'
import * as contrastClassName from '@/className.ts'
import * as contrastCustomPropertyName from '@/customPropertyName.ts'
import type * as contrastExpression from '@/expression.ts'
import type * as contrastExpressionIntern from '@/expressionIntern.ts'
import * as collections from '@intertwine/lib-collection'
import type * as compute from '@intertwine/lib-compute'

export interface Context {
  readonly contrast: Contrast
}

export class Contrast {
  readonly atomClassName =
    new contrastClassName.CustomPropertyNameGenerator(
      contrastClassName.atomNamespace,
    )

  readonly atomIntern = new collections.MultiMemo<
    [
      propertyName: string,
      expressionIntern: contrastExpressionIntern.ExpressionIntern,
    ],
    contrastAtomIntern.AtomIntern
  >(
    (propertyName, expressionIntern) =>
      new contrastAtomIntern.AtomIntern(propertyName, expressionIntern),
  )

  readonly expressionClassName =
    new contrastClassName.CustomPropertyNameGenerator(
      contrastClassName.expressionNamespace,
    )

  readonly expressionCustomPropertyName =
    new contrastCustomPropertyName.CustomPropertyNameGenerator(
      contrastCustomPropertyName.expressionNamespace,
    )

  readonly expressionIntern = new collections.MultiMemo<
    [
      (
        ...args: readonly unknown[]
      ) => contrastExpressionIntern.ExpressionIntern,
      ...unknown[],
    ],
    contrastExpressionIntern.ExpressionIntern
  >((compile, ...args) => compile(...args))

  readonly symbolCustomPropertyName =
    new contrastCustomPropertyName.CustomPropertyNameMemo<symbol>(
      contrastCustomPropertyName.symbolNamespace,
    )
}

export interface CompileContext {
  readonly contrastCompile: ContrastCompile
}

export class ContrastCompile {
  readonly computationCustomPropertyName =
    new contrastCustomPropertyName.CustomPropertyNameMemo<
      compute.Node<contrastExpression.RestrictedExpressionOpt<unknown>>
    >(contrastCustomPropertyName.computationNamespace)
}

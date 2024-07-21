import * as contrastAtomIntern from '@/atomIntern.ts'
import type * as contrastExpressionIntern from '@/expressionIntern.ts'
import * as contrastGeneratedName from '@/generatedName.ts'
import * as collection from '@intertwine/lib-collection'

export interface Context {
  readonly contrast: Contrast
}

export class Contrast {
  readonly atomClassName = contrastGeneratedName.identifierGenerator(
    contrastGeneratedName.atomNamespace,
  )

  readonly atomIntern = new collection.MultiMemo<
    [
      pseudoElement: string | null,
      propertyName: string,
      expressionIntern: contrastExpressionIntern.ExpressionIntern,
    ],
    contrastAtomIntern.AtomIntern
  >(
    (pseudoElement, propertyName, expressionIntern) =>
      new contrastAtomIntern.AtomIntern(
        pseudoElement,
        propertyName,
        expressionIntern,
      ),
  )

  readonly containerName = contrastGeneratedName.identifierMemo<symbol>(
    contrastGeneratedName.containerNamespace,
  )

  readonly expressionClassName = contrastGeneratedName.identifierGenerator(
    contrastGeneratedName.expressionNamespace,
  )

  readonly expressionCustomPropertyName =
    contrastGeneratedName.customPropertyNameGenerator(
      contrastGeneratedName.expressionNamespace,
    )

  readonly expressionIntern = new collection.MultiMemo<
    [
      compile: (
        ...args: readonly unknown[]
      ) => contrastExpressionIntern.ExpressionIntern,
      ...args: readonly unknown[],
    ],
    contrastExpressionIntern.ExpressionIntern
  >((compile, ...args) => compile(...args))

  readonly scopeIntern = new collection.MultiMemo<
    [
      compile: (...args: readonly unknown[]) => string,
      ...args: readonly unknown[],
    ],
    string
  >((compile, ...args) => compile(...args))

  readonly symbolCustomPropertyName =
    contrastGeneratedName.customPropertyNameMemo<symbol>(
      contrastGeneratedName.symbolNamespace,
    )
}

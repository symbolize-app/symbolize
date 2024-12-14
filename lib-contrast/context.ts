import * as contrastAtomIntern from '@/atomIntern.ts'
import type * as contrastExpressionIntern from '@/expressionIntern.ts'
import * as contrastGeneratedName from '@/generatedName.ts'
import * as collection from '@symbolize/lib-collection'

export interface Context {
  readonly contrast: Contrast
}

class Contrast {
  readonly atomClassName = contrastGeneratedName.identifierGenerator(
    contrastGeneratedName.atomNamespace,
  )

  readonly atomIntern = collection.multiMemo<
    [
      pseudoElement: string | null,
      propertyName: string,
      expressionIntern: contrastExpressionIntern.ExpressionIntern,
    ],
    contrastAtomIntern.AtomIntern
  >((pseudoElement, propertyName, expressionIntern) =>
    contrastAtomIntern.atomIntern(
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

  readonly expressionIntern = collection.multiMemo<
    [
      compile: (
        ...args: readonly unknown[]
      ) => contrastExpressionIntern.ExpressionIntern,
      ...args: readonly unknown[],
    ],
    contrastExpressionIntern.ExpressionIntern
  >((compile, ...args) => compile(...args))

  readonly scopeIntern = collection.multiMemo<
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

export type { Contrast }

export function contrast(): Contrast {
  return new Contrast()
}

import * as stylingAtomIntern from '@/atomIntern.ts'
import type * as stylingExpressionIntern from '@/expressionIntern.ts'
import * as stylingGeneratedName from '@/generatedName.ts'
import * as collection from '@symbolize/lib-collection'

export interface Context {
  readonly styling: Styling
}

class Styling {
  readonly atomClassName = stylingGeneratedName.identifierGenerator(
    stylingGeneratedName.atomNamespace,
  )

  readonly atomIntern = collection.multiMemo<
    [
      pseudoElement: string | null,
      propertyName: string,
      expressionIntern: stylingExpressionIntern.ExpressionIntern,
    ],
    stylingAtomIntern.AtomIntern
  >((pseudoElement, propertyName, expressionIntern) =>
    stylingAtomIntern.atomIntern(
      pseudoElement,
      propertyName,
      expressionIntern,
    ),
  )

  readonly containerName = stylingGeneratedName.identifierMemo<symbol>(
    stylingGeneratedName.containerNamespace,
  )

  readonly expressionClassName = stylingGeneratedName.identifierGenerator(
    stylingGeneratedName.expressionNamespace,
  )

  readonly expressionCustomPropertyName =
    stylingGeneratedName.customPropertyNameGenerator(
      stylingGeneratedName.expressionNamespace,
    )

  readonly expressionIntern = collection.multiMemo<
    [
      compile: (
        ...args: readonly unknown[]
      ) => stylingExpressionIntern.ExpressionIntern,
      ...args: readonly unknown[],
    ],
    stylingExpressionIntern.ExpressionIntern
  >((compile, ...args) => compile(...args))

  readonly scopeIntern = collection.multiMemo<
    [
      compile: (...args: readonly unknown[]) => string,
      ...args: readonly unknown[],
    ],
    string
  >((compile, ...args) => compile(...args))

  readonly symbolCustomPropertyName =
    stylingGeneratedName.customPropertyNameMemo<symbol>(
      stylingGeneratedName.symbolNamespace,
    )
}

export type { Styling }

export function styling(): Styling {
  return new Styling()
}

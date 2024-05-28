import type * as contrastAtom from '@/atom.ts'
import * as contrastContext from '@/context.ts'
import type * as contrastExpression from '@/expression.ts'
import type * as contrastRule from '@/rule.ts'
import type * as compute from '@intertwine/lib-compute'

export type Style = readonly (contrastAtom.Atom | Style)[]

export function isStyle(value: contrastAtom.Atom | Style): value is Style {
  return Array.isArray(value)
}

export function compile(
  baseCtx: contrastContext.Context,
  style: Style,
): {
  readonly computationCustomProperties: readonly [
    compute.Node<contrastExpression.RestrictedExpressionOpt<unknown>>,
    string,
  ][]
  readonly rules: readonly contrastRule.Rule[]
} {
  const ctx: contrastContext.CompileContext & contrastContext.Context = {
    ...baseCtx,
    contrastCompile: new contrastContext.ContrastCompile(),
  }
  const rules = [...new Set(compileRules(ctx, style))]
  const computationCustomProperties = [
    ...ctx.contrastCompile.computationCustomPropertyName.entries(),
  ]
  return {
    computationCustomProperties,
    rules,
  }
}

function* compileRules(
  ctx: contrastContext.CompileContext & contrastContext.Context,
  style: Style,
): IterableIterator<contrastRule.Rule> {
  for (const atom of getFinalAtoms(toAtomIterable(style))) {
    const atomIntern = atom.compile(ctx)
    for (const rule of atomIntern.rules(ctx)) {
      yield rule
    }
  }
}

function* toAtomIterable(
  style: Style,
): IterableIterator<contrastAtom.Atom> {
  for (const item of style) {
    if (isStyle(item)) {
      for (const subitem of toAtomIterable(item)) {
        yield subitem
      }
    } else {
      yield item
    }
  }
}

function getFinalAtoms(
  atoms: IterableIterator<contrastAtom.Atom>,
): IterableIterator<contrastAtom.Atom> {
  const finalAtoms = new Map<string | symbol, contrastAtom.Atom>()
  for (const atom of atoms) {
    finalAtoms.set(atom.propertyName, atom)
  }
  return finalAtoms.values()
}

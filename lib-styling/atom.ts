import type * as stylingAtomIntern from '@/atomIntern.ts'
import type * as stylingContext from '@/context.ts'
import * as stylingExpression from '@/expression.ts'
import type * as stylingRule from '@/rule.ts'
import * as collection from '@symbolize/lib-collection'

class Atom {
  constructor(
    readonly pseudoElement: string | null,
    readonly propertyName: string | symbol,
    readonly expressionOpt: stylingExpression.ExpressionOpt<unknown>,
  ) {}

  compile(ctx: stylingContext.Context): stylingAtomIntern.AtomIntern {
    // Note: force a pure expression for pseudo-elements because the
    // pseudo-element selector needs to be the final selector
    const expressionIntern =
      this.pseudoElement === null ?
        stylingExpression.compile(ctx, this.expressionOpt)
      : stylingExpression.compile(ctx, this.expressionOpt).toPure(ctx)
    return ctx.styling.atomIntern.get(
      this.pseudoElement,
      typeof this.propertyName === 'string' ?
        this.propertyName
      : ctx.styling.symbolCustomPropertyName.get(this.propertyName),
      expressionIntern,
    )
  }
}

export type { Atom }

export function atom(
  propertyName: string | symbol,
  value: stylingExpression.ExpressionOpt<unknown>,
): Atom {
  const pseudoElement = null
  return new Atom(pseudoElement, propertyName, value)
}

export function atomWithPseudoElement(
  pseudoElement: string,
  propertyName: string | symbol,
  value: stylingExpression.ExpressionOpt<unknown>,
): Atom {
  return new Atom(pseudoElement, propertyName, value)
}

export type AtomOpt = Atom | readonly AtomOpt[] | null

export function compile(
  ctx: stylingContext.Context,
  atomOpt: AtomOpt,
): IterableIterator<stylingRule.Rule> {
  return new Set(compileRules(ctx, atomOpt)).values()
}

function* compileRules(
  ctx: stylingContext.Context,
  atomOpt: AtomOpt,
): IterableIterator<stylingRule.Rule> {
  for (const atom of getFinalAtoms(toAtomIterable(atomOpt))) {
    const atomIntern = atom.compile(ctx)
    for (const rule of atomIntern.rules(ctx)) {
      yield rule
    }
  }
}

export function* toAtomIterable(atomOpt: AtomOpt): IterableIterator<Atom> {
  if (isAtomOptArray(atomOpt)) {
    for (const item of atomOpt) {
      for (const subitem of toAtomIterable(item)) {
        yield subitem
      }
    }
  } else if (atomOpt !== null) {
    yield atomOpt
  }
}

function isAtomOptArray(atomOpt: AtomOpt): atomOpt is readonly AtomOpt[] {
  return Array.isArray(atomOpt)
}

function* getFinalAtoms(
  atoms: IterableIterator<Atom>,
): IterableIterator<Atom> {
  const finalAtoms = collection.memo<
    string | null,
    Map<string | symbol, Atom>
  >(() => new Map())
  for (const atom of atoms) {
    finalAtoms.get(atom.pseudoElement).set(atom.propertyName, atom)
  }
  for (const group of finalAtoms.values()) {
    for (const atom of group.values()) {
      yield atom
    }
  }
}

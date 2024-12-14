import type * as contrastAtomIntern from '@/atomIntern.ts'
import type * as contrastContext from '@/context.ts'
import * as contrastExpression from '@/expression.ts'
import type * as contrastRule from '@/rule.ts'
import * as collection from '@symbolize/lib-collection'

class Atom {
  constructor(
    readonly pseudoElement: string | null,
    readonly propertyName: string | symbol,
    readonly expressionOpt: contrastExpression.ExpressionOpt<unknown>,
  ) {}

  compile(ctx: contrastContext.Context): contrastAtomIntern.AtomIntern {
    // Note: force a pure expression for pseudo-elements because the
    // pseudo-element selector needs to be the final selector
    const expressionIntern =
      this.pseudoElement === null ?
        contrastExpression.compile(ctx, this.expressionOpt)
      : contrastExpression.compile(ctx, this.expressionOpt).toPure(ctx)
    return ctx.contrast.atomIntern.get(
      this.pseudoElement,
      typeof this.propertyName === 'string' ?
        this.propertyName
      : ctx.contrast.symbolCustomPropertyName.get(this.propertyName),
      expressionIntern,
    )
  }
}

export type { Atom }

export function atom(
  propertyName: string | symbol,
  value: contrastExpression.ExpressionOpt<unknown>,
): Atom {
  const pseudoElement = null
  return new Atom(pseudoElement, propertyName, value)
}

export function atomWithPseudoElement(
  pseudoElement: string,
  propertyName: string | symbol,
  value: contrastExpression.ExpressionOpt<unknown>,
): Atom {
  return new Atom(pseudoElement, propertyName, value)
}

export type AtomOpt = Atom | readonly AtomOpt[] | null

export function compile(
  ctx: contrastContext.Context,
  atomOpt: AtomOpt,
): IterableIterator<contrastRule.Rule> {
  return new Set(compileRules(ctx, atomOpt)).values()
}

function* compileRules(
  ctx: contrastContext.Context,
  atomOpt: AtomOpt,
): IterableIterator<contrastRule.Rule> {
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

import type * as contrastAtomIntern from '@/atomIntern.ts'
import type * as contrastContext from '@/context.ts'
import * as contrastExpression from '@/expression.ts'
import type * as contrastRule from '@/rule.ts'

export class Atom {
  constructor(
    readonly propertyName: string | symbol,
    readonly expressionOpt: contrastExpression.ExpressionOpt<unknown>,
  ) {}

  compile(ctx: contrastContext.Context): contrastAtomIntern.AtomIntern {
    return ctx.contrast.atomIntern.get(
      typeof this.propertyName === 'string' ?
        this.propertyName
      : ctx.contrast.symbolCustomPropertyName.get(this.propertyName),
      contrastExpression.compile(ctx, this.expressionOpt),
    )
  }
}

export function atom(
  propertyName: string | symbol,
  value: contrastExpression.ExpressionOpt<unknown>,
): Atom {
  return new Atom(propertyName, value)
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

function* toAtomIterable(atomOpt: AtomOpt): IterableIterator<Atom> {
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

function getFinalAtoms(
  atoms: IterableIterator<Atom>,
): IterableIterator<Atom> {
  const finalAtoms = new Map<string | symbol, Atom>()
  for (const atom of atoms) {
    finalAtoms.set(atom.propertyName, atom)
  }
  return finalAtoms.values()
}

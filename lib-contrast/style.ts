import type * as contrastAtom from '@/atom.ts'
import type * as contrastContext from '@/context.ts'
import type * as contrastRule from '@/rule.ts'

export type Style = readonly (contrastAtom.Atom | Style)[]

export function isStyle(value: contrastAtom.Atom | Style): value is Style {
  return Array.isArray(value)
}

export function* compile(
  ctx: contrastContext.Context,
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

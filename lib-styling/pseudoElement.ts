import * as stylingAtom from '@/atom.ts'

export function after(atomOpt: stylingAtom.AtomOpt): stylingAtom.AtomOpt {
  return pseudoElement('after', atomOpt)
}

export function before(atomOpt: stylingAtom.AtomOpt): stylingAtom.AtomOpt {
  return pseudoElement('before', atomOpt)
}

function pseudoElement(
  name: string,
  atomOpt: stylingAtom.AtomOpt,
): stylingAtom.AtomOpt {
  return [...stylingAtom.toAtomIterable(atomOpt)].map((atom) => {
    if (atom.pseudoElement !== null) {
      throw new Error(
        `${typeof atom.propertyName === 'string' ? atom.propertyName : 'var'} atom already has ::${atom.pseudoElement} pseudo-element`,
      )
    } else {
      return stylingAtom.atomWithPseudoElement(
        name,
        atom.propertyName,
        atom.expressionOpt,
      )
    }
  })
}

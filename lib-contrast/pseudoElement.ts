import * as contrastAtom from '@/atom.ts'

export function after(
  atomOpt: contrastAtom.AtomOpt,
): contrastAtom.AtomOpt {
  return pseudoElement('after', atomOpt)
}

export function before(
  atomOpt: contrastAtom.AtomOpt,
): contrastAtom.AtomOpt {
  return pseudoElement('before', atomOpt)
}

function pseudoElement(
  name: string,
  atomOpt: contrastAtom.AtomOpt,
): contrastAtom.AtomOpt {
  return [...contrastAtom.toAtomIterable(atomOpt)].map((atom) => {
    if (atom.pseudoElement !== null) {
      throw new Error(
        `${typeof atom.propertyName === 'string' ? atom.propertyName : 'var'} atom already has ::${atom.pseudoElement} pseudo-element`,
      )
    } else {
      return contrastAtom.atomWithPseudoElement(
        name,
        atom.propertyName,
        atom.expressionOpt,
      )
    }
  })
}

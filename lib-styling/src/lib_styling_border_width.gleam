import lib_styling_atom as atom
import lib_styling_values as values

pub fn t(value: values.LengthOnly) -> atom.AtomOpt {
  atom.atom("border-top-width", values.length_only(value))
}

pub fn r(value: values.LengthOnly) -> atom.AtomOpt {
  atom.atom("border-right-width", values.length_only(value))
}

pub fn b(value: values.LengthOnly) -> atom.AtomOpt {
  atom.atom("border-bottom-width", values.length_only(value))
}

pub fn l(value: values.LengthOnly) -> atom.AtomOpt {
  atom.atom("border-left-width", values.length_only(value))
}

pub fn tb(value: values.LengthOnly) -> atom.AtomOpt {
  atom.Many([t(value), b(value)])
}

pub fn rl(value: values.LengthOnly) -> atom.AtomOpt {
  atom.Many([r(value), l(value)])
}

pub fn trbl(value: values.LengthOnly) -> atom.AtomOpt {
  atom.Many([t(value), r(value), b(value), l(value)])
}

pub fn os(value: values.LengthOnly) -> atom.AtomOpt {
  atom.atom("border-block-start-width", values.length_only(value))
}

pub fn oe(value: values.LengthOnly) -> atom.AtomOpt {
  atom.atom("border-block-end-width", values.length_only(value))
}

pub fn is(value: values.LengthOnly) -> atom.AtomOpt {
  atom.atom("border-inline-start-width", values.length_only(value))
}

pub fn ie(value: values.LengthOnly) -> atom.AtomOpt {
  atom.atom("border-inline-end-width", values.length_only(value))
}

pub fn o(value: values.LengthOnly) -> atom.AtomOpt {
  atom.Many([os(value), oe(value)])
}

pub fn i(value: values.LengthOnly) -> atom.AtomOpt {
  atom.Many([is(value), ie(value)])
}

pub fn oi(value: values.LengthOnly) -> atom.AtomOpt {
  atom.Many([os(value), oe(value), is(value), ie(value)])
}

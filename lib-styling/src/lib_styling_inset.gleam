import lib_styling_atom as atom
import lib_styling_values as values

pub fn t(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("top", values.length_percentage(value))
}

pub fn r(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("right", values.length_percentage(value))
}

pub fn b(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("bottom", values.length_percentage(value))
}

pub fn l(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("left", values.length_percentage(value))
}

pub fn tb(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([t(value), b(value)])
}

pub fn rl(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([r(value), l(value)])
}

pub fn trbl(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([t(value), r(value), b(value), l(value)])
}

pub fn os(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("inset-block-start", values.length_percentage(value))
}

pub fn oe(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("inset-block-end", values.length_percentage(value))
}

pub fn is(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("inset-inline-start", values.length_percentage(value))
}

pub fn ie(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("inset-inline-end", values.length_percentage(value))
}

pub fn o(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([os(value), oe(value)])
}

pub fn i(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([is(value), ie(value)])
}

pub fn oi(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([os(value), oe(value), is(value), ie(value)])
}

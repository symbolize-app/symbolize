import lib_styling_atom as atom
import lib_styling_values as values

pub fn t(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.atom("margin-top", values.length_percentage_auto(value))
}

pub fn r(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.atom("margin-right", values.length_percentage_auto(value))
}

pub fn b(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.atom("margin-bottom", values.length_percentage_auto(value))
}

pub fn l(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.atom("margin-left", values.length_percentage_auto(value))
}

pub fn tb(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.Many([t(value), b(value)])
}

pub fn rl(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.Many([r(value), l(value)])
}

pub fn trbl(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.Many([t(value), r(value), b(value), l(value)])
}

pub fn os(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.atom("margin-block-start", values.length_percentage_auto(value))
}

pub fn oe(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.atom("margin-block-end", values.length_percentage_auto(value))
}

pub fn is(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.atom("margin-inline-start", values.length_percentage_auto(value))
}

pub fn ie(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.atom("margin-inline-end", values.length_percentage_auto(value))
}

pub fn o(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.Many([os(value), oe(value)])
}

pub fn i(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.Many([is(value), ie(value)])
}

pub fn oi(value: values.LengthPercentageAuto) -> atom.AtomOpt {
  atom.Many([os(value), oe(value), is(value), ie(value)])
}

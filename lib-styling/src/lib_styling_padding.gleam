import lib_styling_atom as atom
import lib_styling_values as values

pub fn t(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("padding-top", values.length_percentage(value))
}

pub fn r(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("padding-right", values.length_percentage(value))
}

pub fn b(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("padding-bottom", values.length_percentage(value))
}

pub fn l(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("padding-left", values.length_percentage(value))
}

pub fn top(value: values.LengthPercentage) -> atom.AtomOpt {
  t(value)
}

pub fn right(value: values.LengthPercentage) -> atom.AtomOpt {
  r(value)
}

pub fn bottom(value: values.LengthPercentage) -> atom.AtomOpt {
  b(value)
}

pub fn left(value: values.LengthPercentage) -> atom.AtomOpt {
  l(value)
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
  atom.atom("padding-block-start", values.length_percentage(value))
}

pub fn oe(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("padding-block-end", values.length_percentage(value))
}

pub fn is(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("padding-inline-start", values.length_percentage(value))
}

pub fn ie(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("padding-inline-end", values.length_percentage(value))
}

pub fn block_start(value: values.LengthPercentage) -> atom.AtomOpt {
  os(value)
}

pub fn block_end(value: values.LengthPercentage) -> atom.AtomOpt {
  oe(value)
}

pub fn inline_start(value: values.LengthPercentage) -> atom.AtomOpt {
  is(value)
}

pub fn inline_end(value: values.LengthPercentage) -> atom.AtomOpt {
  ie(value)
}

pub fn outer(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([os(value), oe(value)])
}

pub fn inner(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([is(value), ie(value)])
}

pub fn outer_inner(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([
    os(value),
    oe(value),
    is(value),
    ie(value),
  ])
}

pub fn oi(value: values.LengthPercentage) -> atom.AtomOpt {
  outer_inner(value)
}

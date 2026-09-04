import lib_styling_atom as atom
import lib_styling_values as values

pub fn mode(value: values.BoxSizing) -> atom.AtomOpt {
  atom.atom("box-sizing", values.box_sizing(value))
}

pub fn min_width(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("min-width", values.length_percentage(value))
}

// Source namespace spelling: styling.size.min.w(...).
pub fn min_w(value: values.LengthPercentage) -> atom.AtomOpt {
  min_width(value)
}

pub fn min_height(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("min-height", values.length_percentage(value))
}

// Source namespace spelling: styling.size.min.h(...).
pub fn min_h(value: values.LengthPercentage) -> atom.AtomOpt {
  min_height(value)
}

pub fn min_width_height(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([min_width(value), min_height(value)])
}

// Source namespace spelling: styling.size.min.wh(...).
pub fn min_wh(value: values.LengthPercentage) -> atom.AtomOpt {
  min_width_height(value)
}

pub fn min_outer(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("min-block-size", values.length_percentage(value))
}

// Source namespace spelling: styling.size.min.o(...).
pub fn min_o(value: values.LengthPercentage) -> atom.AtomOpt {
  min_outer(value)
}

pub fn min_inner(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("min-inline-size", values.length_percentage(value))
}

// Source namespace spelling: styling.size.min.i(...).
pub fn min_i(value: values.LengthPercentage) -> atom.AtomOpt {
  min_inner(value)
}

pub fn min_outer_inner(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([min_outer(value), min_inner(value)])
}

// Source namespace spelling: styling.size.min.oi(...).
pub fn min_oi(value: values.LengthPercentage) -> atom.AtomOpt {
  min_outer_inner(value)
}

pub fn max_width(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("max-width", values.length_percentage(value))
}

// Source namespace spelling: styling.size.max.w(...).
pub fn max_w(value: values.LengthPercentage) -> atom.AtomOpt {
  max_width(value)
}

pub fn max_height(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("max-height", values.length_percentage(value))
}

// Source namespace spelling: styling.size.max.h(...).
pub fn max_h(value: values.LengthPercentage) -> atom.AtomOpt {
  max_height(value)
}

pub fn max_width_height(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([max_width(value), max_height(value)])
}

// Source namespace spelling: styling.size.max.wh(...).
pub fn max_wh(value: values.LengthPercentage) -> atom.AtomOpt {
  max_width_height(value)
}

pub fn max_outer(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("max-block-size", values.length_percentage(value))
}

// Source namespace spelling: styling.size.max.o(...).
pub fn max_o(value: values.LengthPercentage) -> atom.AtomOpt {
  max_outer(value)
}

pub fn max_inner(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("max-inline-size", values.length_percentage(value))
}

// Source namespace spelling: styling.size.max.i(...).
pub fn max_i(value: values.LengthPercentage) -> atom.AtomOpt {
  max_inner(value)
}

pub fn max_outer_inner(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([max_outer(value), max_inner(value)])
}

// Source namespace spelling: styling.size.max.oi(...).
pub fn max_oi(value: values.LengthPercentage) -> atom.AtomOpt {
  max_outer_inner(value)
}

pub fn width(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("width", values.length_percentage(value))
}

// Source namespace spelling: styling.size.w(...).
pub fn w(value: values.LengthPercentage) -> atom.AtomOpt {
  width(value)
}

pub fn height(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("height", values.length_percentage(value))
}

// Source namespace spelling: styling.size.h(...).
pub fn h(value: values.LengthPercentage) -> atom.AtomOpt {
  height(value)
}

pub fn width_height(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([width(value), height(value)])
}

// Source namespace spelling: styling.size.wh(...).
pub fn wh(value: values.LengthPercentage) -> atom.AtomOpt {
  width_height(value)
}

pub fn outer(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("block-size", values.length_percentage(value))
}

// Source namespace spelling: styling.size.o(...).
pub fn o(value: values.LengthPercentage) -> atom.AtomOpt {
  outer(value)
}

pub fn inner(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.atom("inline-size", values.length_percentage(value))
}

// Source namespace spelling: styling.size.i(...).
pub fn i(value: values.LengthPercentage) -> atom.AtomOpt {
  inner(value)
}

pub fn outer_inner(value: values.LengthPercentage) -> atom.AtomOpt {
  atom.Many([outer(value), inner(value)])
}

// Source namespace spelling: styling.size.oi(...).
pub fn oi(value: values.LengthPercentage) -> atom.AtomOpt {
  outer_inner(value)
}

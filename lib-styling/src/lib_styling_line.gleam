import lib_styling_atom as atom
import lib_styling_values as values

pub fn height(value: values.LineHeight) -> atom.AtomOpt {
  atom.atom("line-height", values.line_height(value))
}

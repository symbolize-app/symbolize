import lib_styling_atom as atom
import lib_styling_font as font

// The source exposes these operations under styling.font.variant. Gleam
// keeps the nested namespace as a sibling module while leaving the value
// families in lib_styling_font, where callers can use one shared type name.
pub fn ligatures(value: font.LigatureValue) -> atom.AtomOpt {
  font.variant_ligatures(value)
}

pub fn numeric(value: font.NumericValue) -> atom.AtomOpt {
  font.variant_numeric(value)
}

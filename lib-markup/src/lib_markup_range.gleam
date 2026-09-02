import lib_markup_fragment as fragment

// The source module accepts one object with a required `content` property.
// Keep the common fragment input type here so ranges compose with every
// existing fragment-producing API without introducing another nested type.
pub type RangeAttrs {
  RangeAttrs(content: List(fragment.FragmentInput))
}

pub fn range(attrs: RangeAttrs) -> fragment.Fragment {
  let RangeAttrs(content: content) = attrs
  fragment.range(content)
}

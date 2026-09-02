import lib_dataflow as dataflow
import lib_markup_fragment as fragment

// The source text helper accepts a NodeOpt rather than a pre-evaluated value,
// so the same fragment handles literal and reactive text. Gleam makes that
// choice explicit at the call site with dataflow.literal or dataflow.mutation.
pub fn text(content: dataflow.NodeOpt(String)) -> fragment.Fragment {
  fragment.reactive_text(content)
}

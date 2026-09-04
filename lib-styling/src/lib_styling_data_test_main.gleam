import gleam/io
import lib_styling_data as data

pub fn main() {
  assert data.length_text(data.length(1.5, data.Cm)) == "1.5cm"
  assert data.length_text(data.em(2.0)) == "2em"
  assert data.length_text(data.px(0.0)) == "0px"
  assert data.length_text(data.length(1.0, data.Q)) == "1Q"
  assert data.length_text(data.length(3.0, data.Dvmax)) == "3dvmax"

  assert data.percentage_text(data.pct(12.5)) == "12.5%"
  assert data.angle_text(data.deg(45.0)) == "45deg"
  assert data.angle_text(data.angle(0.25, data.Turn)) == "0.25turn"

  assert data.string_text(data.string_literal("hello")) == "\"hello\""
  assert data.string_text(data.attr("data-label")) == "attr(data-label)"

  assert data.svg_paint_text(data.ContextFill) == "context-fill"
  assert data.svg_paint_text(data.ContextStroke) == "context-stroke"
  assert data.svg_paint_text(data.NoPaint) == "none"

  io.println("lib-styling data Gleam parity tests passed")
}

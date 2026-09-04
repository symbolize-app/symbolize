import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import lib_styling as styling
import lib_styling_accent as accent
import lib_styling_atom as atom
import lib_styling_background as background
import lib_styling_border_style as border_style
import lib_styling_border_width as border_width
import lib_styling_caret as caret
import lib_styling_container as container_dsl
import lib_styling_content as content_dsl
import lib_styling_context as context
import lib_styling_data as data
import lib_styling_expr as expr
import lib_styling_fill as fill
import lib_styling_font as font
import lib_styling_font_variant as font_variant
import lib_styling_gradient as gradient
import lib_styling_hyphen as hyphen
import lib_styling_inset as inset
import lib_styling_isolation as isolation
import lib_styling_line as line
import lib_styling_margin as margin
import lib_styling_media as media
import lib_styling_overflow as overflow
import lib_styling_padding as padding
import lib_styling_pointer as pointer
import lib_styling_position as position
import lib_styling_punctuation as punctuation
import lib_styling_select as select
import lib_styling_size as size
import lib_styling_support as support
import lib_styling_text as styling_text
import lib_styling_typed_expr as typed_expr
import lib_styling_values as values
import lib_styling_var as var_dsl

pub fn run(done: fn() -> Nil) {
  let red =
    expr.rgb(
      expr.pct(data.pct(100.0)),
      expr.pct(data.pct(0.0)),
      expr.pct(data.pct(0.0)),
    )
  let typed_red =
    typed_expr.rgb(
      typed_expr.pct(data.pct(100.0)),
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(0.0)),
    )
  let style = atom.atom("background-color", red)
  let #(context, rules, class_names) = atom.compile(context.styling(), style)
  assert class_names == ["a0"]
  assert rules == [context.Rule("a0", ".a0{background-color:rgb(100% 0% 0%)}")]
  let #(_context, repeated_rules, repeated_class_names) =
    atom.compile(context, style)
  assert repeated_class_names == ["a0"]
  assert repeated_rules == rules

  let blue =
    expr.rgb(
      expr.pct(data.pct(0.0)),
      expr.pct(data.pct(0.0)),
      expr.pct(data.pct(2.0)),
    )
  let #(_context, rules, class_names) =
    atom.compile(
      context.styling(),
      atom.Many([style, atom.atom("background-color", blue)]),
    )
  assert class_names == ["a0"]
  assert rules == [context.Rule("a0", ".a0{background-color:rgb(0% 0% 2%)}")]

  let hover = select.match(select.hover(), red)
  let #(_context, rules, class_names) =
    atom.compile(context.styling(), atom.atom("background-color", hover))
  assert class_names == ["a0"]
  assert rules
    == [
      context.Rule(
        "a0",
        ".a0{&:where(:hover){background-color:rgb(100% 0% 0%)}}",
      ),
    ]

  let media_value =
    media.match(
      media.Screen,
      media.min_w(values.LengthPercentageLength(data.px(300.0))),
      expr.keyword("scroll"),
    )
  let #(_context, rules, _) =
    atom.compile(context.styling(), atom.atom("overflow-x", media_value))
  assert rules
    == [
      context.Rule(
        "a0",
        ".a0{@media only screen and (min-width: 300px){overflow-x:scroll}}",
      ),
    ]

  let #(context, variable_id) = context.variable(context.styling())
  let variable =
    expr.cascade(expr.pct(data.pct(0.0)), [
      select.match(select.hover(), expr.pct(data.pct(1.0))),
    ])
  let #(_context, rules, _) =
    atom.compile(
      context,
      atom.atom(
        "background-color",
        expr.rgb(expr.pct(data.pct(0.0)), expr.variable(variable_id), variable),
      ),
    )
  assert rules
    == [
      context.Rule("e0", ".e0{--e0:0%;&:where(:hover){--e0:1%}}"),
      context.Rule("a0", ".a0{background-color:rgb(0% var(--s0) var(--e0))}"),
    ]

  let #(_context, rules, class_names) =
    atom.compile(
      context.styling(),
      atom.Many([
        background.color(background.ColorExpression(typed_red)),
        overflow.x(overflow.Scroll),
        padding.outer_inner(values.LengthPercentageLength(data.px(2.0))),
      ]),
    )
  assert class_names == ["a0", "a1", "a2", "a3", "a4", "a5"]
  assert rules
    == [
      context.Rule("a0", ".a0{background-color:rgb(100% 0% 0%)}"),
      context.Rule("a1", ".a1{overflow-x:scroll}"),
      context.Rule("a2", ".a2{padding-block-start:2px}"),
      context.Rule("a3", ".a3{padding-block-end:2px}"),
      context.Rule("a4", ".a4{padding-inline-start:2px}"),
      context.Rule("a5", ".a5{padding-inline-end:2px}"),
    ]

  let supported =
    support.match(
      support.code(atom.atom("overflow-x", expr.keyword("scroll"))),
      expr.keyword("scroll"),
    )
  let #(_context, rules, class_names) =
    atom.compile(context.styling(), atom.atom("overflow-x", supported))
  assert class_names == ["a0"]
  assert rules
    == [
      context.Rule(
        "a0",
        ".a0{@supports (overflow-x:scroll){overflow-x:scroll}}",
      ),
    ]

  let #(context, container) = container_dsl.build(context.styling())
  let named: styling.Expression(styling.ContainerName) =
    container_dsl.value(container)
  let scoped =
    container_dsl.match_named_typed(
      container,
      container_dsl.min_i(values.LengthPercentageLength(data.px(300.0))),
      typed_red,
    )
  let #(_context, rules, class_names) =
    atom.compile(
      context,
      atom.Many([
        container_dsl.name(named, []),
        container_dsl.type_(container_dsl.InlineSize),
        background.color(background.ColorExpression(scoped)),
      ]),
    )
  assert class_names == ["a0", "a1", "a2"]
  assert rules
    == [
      context.Rule("a0", ".a0{container-name:r0}"),
      context.Rule("a1", ".a1{container-type:inline-size}"),
      context.Rule(
        "a2",
        ".a2{@container r0 (min-inline-size: 300px){background-color:rgb(100% 0% 0%)}}",
      ),
    ]

  let #(_context, rules, _) =
    atom.compile(
      context.styling(),
      content_dsl.content(
        content_dsl.Expression(
          typed_expr.css_string(data.string_literal("\"'")),
        ),
        [],
      ),
    )
  assert rules
    == [
      context.Rule("a0", ".a0{content:\"\\\"'\"}"),
    ]

  let #(_context, rules, _) =
    atom.compile(
      context.styling(),
      background.color(
        background.ColorExpression(typed_expr.rgb_alpha(
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.5)),
        )),
      ),
    )
  assert rules
    == [
      context.Rule("a0", ".a0{background-color:rgb(0% 0% 0% / 0.5%)}"),
    ]

  let #(_context, rules, _) =
    atom.compile(
      context.styling(),
      background.size(background.Single(background.Cover), [
        background.Pair(background.Length(data.px(1.0)), background.Auto),
      ]),
    )
  assert rules
    == [
      context.Rule("a0", ".a0{background-size:cover,1px auto}"),
    ]

  let number_value =
    expr.add(expr.number(1.0), [expr.number(2.0), expr.number(3.0)])
  let #(_context, rules, _) =
    atom.compile(context.styling(), atom.atom("--s0", number_value))
  assert rules
    == [
      context.Rule("a0", ".a0{--s0:calc(1 + 2 + 3)}"),
    ]

  variable_identity()
  container_identity()

  let first_gradient =
    gradient.linear(
      gradient.AngleExpression(typed_expr.angle(data.deg(1.0))),
      gradient.value(
        gradient.ColorExpression(typed_expr.rgb(
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(2.0)),
        )),
      ),
      [
        gradient.plain(
          gradient.value(
            gradient.ColorExpression(typed_expr.rgb(
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(3.0)),
            )),
          ),
        ),
      ],
    )
  let second_gradient =
    gradient.linear_stops(
      gradient.Angle(data.deg(4.0)),
      gradient.value(
        gradient.ColorExpression(typed_expr.rgb(
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(5.0)),
        )),
      ),
      [
        gradient.hint(
          gradient.PercentageExpression(typed_expr.pct(data.pct(6.0))),
          gradient.stop(
            gradient.ColorExpression(typed_expr.rgb(
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(7.0)),
            )),
            gradient.PercentageExpression(typed_expr.pct(data.pct(8.0))),
            Some(gradient.PercentageExpression(typed_expr.pct(data.pct(9.0)))),
          ),
        ),
      ],
    )
  let #(_context, rules, _) =
    atom.compile(
      context.styling(),
      gradient.background_image(first_gradient, [second_gradient]),
    )
  assert rules
    == [
      context.Rule(
        "a0",
        ".a0{background-image:linear-gradient(1deg,rgb(0% 0% 2%),rgb(0% 0% 3%)),linear-gradient(4deg,rgb(0% 0% 5%),6%,rgb(0% 0% 7%) 8% 9%)}",
      ),
    ]

  // `Gradient` is an expression value, so the source's generic cascade can
  // be composed and passed to the property without a raw-expression escape.
  let cascaded_gradient = typed_expr.cascade(first_gradient, [second_gradient])
  let #(_context, rules, _) =
    atom.compile(context.styling(), background.image(cascaded_gradient, []))
  assert rules
    == [
      context.Rule(
        "e0",
        ".e0{--e0:linear-gradient(1deg,rgb(0% 0% 2%),rgb(0% 0% 3%));--e0:linear-gradient(4deg,rgb(0% 0% 5%),6%,rgb(0% 0% 7%) 8% 9%)}",
      ),
      context.Rule("a0", ".a0{background-image:var(--e0)}"),
    ]

  let media_value =
    media.match(
      media.Screen,
      media.and(media.any_input_hover_value(media.NoHover), [
        media.hint_color_scheme(media.Dark),
      ]),
      expr.keyword("scroll"),
    )
  let #(_context, rules, _) =
    atom.compile(context.styling(), atom.atom("overflow-x", media_value))
  assert rules
    == [
      context.Rule(
        "a0",
        ".a0{@media only screen and ((any-hover: none) and (prefers-color-scheme: dark)){overflow-x:scroll}}",
      ),
    ]

  let #(_context, rules, class_names) =
    atom.compile(
      context.styling(),
      atom.Many([
        styling.before(atom.atom("color", expr.keyword("red"))),
        atom.atom("background", expr.keyword("blue")),
        styling.after(atom.atom("border", expr.keyword("green"))),
        atom.atom("display", expr.keyword("block")),
      ]),
    )
  assert class_names == ["a0", "a1", "a2", "a3"]
  assert rules
    == [
      context.Rule("a0", ".a0::before{color:red}"),
      context.Rule("a1", ".a1{background:blue}"),
      context.Rule("a2", ".a2{display:block}"),
      context.Rule("a3", ".a3::after{border:green}"),
    ]

  property_surface()
  typed_property_surface()
  typed_variable_surface()
  color_function_surface()
  typed_expression_surface()
  public_root_typed_surface()
  math_surface()
  expression_surface()
  select_surface()
  support_surface()
  atom_surface()
  media_surface()
  container_surface()
  pseudo_surface()
  support_identity()
  hexadecimal_generated_names()

  io.println("lib-styling Gleam parity tests passed")
  done()
}

pub fn main() {
  run(fn() { Nil })
}

// The source's background.color accepts Expression<Color>, while an
// Expression<Angle> or Expression<Length> is rejected before compilation.
// Keep this call path separate from the legacy raw parity fixtures so the
// compiler, not a runtime convention, demonstrates the property boundary.
fn typed_property_surface() {
  let red = typed_expr.pct(data.pct(100.0))
  let green = typed_expr.pct(data.pct(0.0))
  let blue = typed_expr.pct(data.pct(50.0))
  let color = typed_expr.rgb(red, green, blue)
  assert_single(
    background.color_typed(color),
    "background-color:rgb(100% 0% 50%)",
  )
  assert_single(
    background.color_typed(select.match_typed(select.hover(), color)),
    "&:where(:hover){background-color:rgb(100% 0% 50%)}",
  )
  assert_scoped(
    background.color_typed(media.match_typed(
      media.All,
      media.reduced_motion(),
      color,
    )),
    ".a0{@media only all and (prefers-reduced-motion: reduce){background-color:rgb(100% 0% 50%)}}",
  )
  let supported =
    support.match_typed(
      support.code(atom.atom("color", expr.keyword("red"))),
      color,
    )
  assert_scoped(
    background.color_typed(supported),
    ".a0{@supports (color:red){background-color:rgb(100% 0% 50%)}}",
  )
  let #(_context, named) = container_dsl.build(context.styling())
  assert_scoped(
    background.color_typed(container_dsl.match_named_typed(
      named,
      container_dsl.orientation(container_dsl.Landscape),
      color,
    )),
    ".a0{@container r0 (orientation: landscape){background-color:rgb(100% 0% 50%)}}",
  )
}

// Mirrors the original variable-operation fixtures with the typed operations. The
// value parameter is inferred once from the set/get uses and is preserved
// through the opaque handle rather than being carried as an untyped raw
// expression.
fn typed_variable_surface() {
  let #(context, first) = var_dsl.build(context.styling())
  let #(context, second) = var_dsl.build(context)
  let color =
    typed_expr.rgb(
      typed_expr.pct(data.pct(0.0)),
      var_dsl.get_typed(first),
      var_dsl.get_typed(second),
    )
  let #(_context, rules, class_names) =
    atom.compile(
      context,
      atom.Many([
        var_dsl.set_typed(first, typed_expr.pct(data.pct(1.0))),
        var_dsl.set_typed(second, typed_expr.pct(data.pct(2.0))),
        background.color_typed(color),
      ]),
    )
  assert class_names == ["a0", "a1", "a2"]
  assert rules
    == [
      context.Rule("a0", ".a0{--s0:1%}"),
      context.Rule("a1", ".a1{--s1:2%}"),
      context.Rule("a2", ".a2{background-color:rgb(0% var(--s0) var(--s1))}"),
    ]

  let #(context, variable) = var_dsl.build(context.styling())
  let fallback =
    typed_expr.cascade(typed_expr.pct(data.pct(0.0)), [
      select.match_typed(select.hover(), typed_expr.pct(data.pct(1.0))),
    ])
  let color =
    typed_expr.rgb(
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(0.0)),
      var_dsl.or_typed(variable, fallback),
    )
  let #(_context, rules, class_names) =
    atom.compile(context, background.color_typed(color))
  assert class_names == ["e0", "a0"]
  assert rules
    == [
      context.Rule("e0", ".e0{--e0:0%;&:where(:hover){--e0:1%}}"),
      context.Rule(
        "a0",
        ".a0{background-color:rgb(0% 0% var(--s0, var(--e0)))}",
      ),
    ]
}

// Mirrors the color constructors in the original styling data source. The source
// accepts three or four typed expression arguments and uses a comma-separated
// light-dark function; these assertions keep those exact emitted forms in the
// Gleam-owned runner.
fn color_function_surface() {
  assert_single(
    background.color(
      background.ColorExpression(typed_expr.hsl(
        typed_expr.angle(data.deg(120.0)),
        typed_expr.pct(data.pct(50.0)),
        typed_expr.pct(data.pct(25.0)),
      )),
    ),
    "background-color:hsl(120deg 50% 25%)",
  )
  assert_single(
    background.color(
      background.ColorExpression(typed_expr.hsl_alpha(
        typed_expr.angle(data.deg(120.0)),
        typed_expr.pct(data.pct(50.0)),
        typed_expr.pct(data.pct(25.0)),
        typed_expr.pct(data.pct(0.5)),
      )),
    ),
    "background-color:hsl(120deg 50% 25% / 0.5%)",
  )
  assert_single(
    background.color(
      background.ColorExpression(typed_expr.light_dark(
        typed_expr.keyword("white"),
        typed_expr.keyword("black"),
      )),
    ),
    "background-color:light-dark(white,black)",
  )
}

// The source Expression<Value> generic is checked independently from the
// legacy untyped compiler surface. A wrong argument type (for example an
// angle in rgb) is rejected by Gleam before the expression reaches the atom
// compiler.
fn typed_expression_surface() {
  let red = typed_expr.pct(data.pct(100.0))
  let green = typed_expr.pct(data.pct(0.0))
  let blue = typed_expr.pct(data.pct(0.0))
  let color = typed_expr.rgb(red, green, blue)
  assert_single(
    atom.atom("background-color", typed_expr.erase(color)),
    "background-color:rgb(100% 0% 0%)",
  )

  let total =
    typed_expr.add(typed_expr.length(data.px(1.0)), [
      typed_expr.length(data.px(2.0)),
    ])
  assert_single(
    atom.atom("width", typed_expr.erase(total)),
    "width:calc(1px + 2px)",
  )
}

// The package root mirrors the source index. Its expression type is now
// parameterized, so source-shaped root constructors retain their CSS value
// family through math and color composition before the atom boundary erases
// it.
fn public_root_typed_surface() {
  let total = styling.add(styling.px(1.0), [styling.px(2.0)])
  assert_single(styling.atom("width", total), "width:calc(1px + 2px)")

  let color =
    styling.rgb(styling.pct(100.0), styling.pct(0.0), styling.pct(50.0))
  assert_single(
    styling.atom("background-color", color),
    "background-color:rgb(100% 0% 50%)",
  )
}

// Mirrors the positive atom fixtures in the original styling test source. The nested
// AtomOpt form is kept explicit here so its flattening and last-value-wins
// behavior remain source-backed rather than inferred from the larger surface
// fixture above.
fn atom_surface() {
  let first =
    typed_expr.rgb(
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(1.0)),
    )
  let second =
    typed_expr.rgb(
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(2.0)),
    )
  let third =
    typed_expr.rgb(
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(3.0)),
    )
  let fourth =
    typed_expr.rgb(
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(4.0)),
    )
  let first_paint =
    typed_expr.rgb_paint(
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(1.0)),
    )
  let #(_, rules, class_names) =
    atom.compile(
      context.styling(),
      atom.Many([
        background.color(background.ColorExpression(first)),
        background.color(background.ColorExpression(second)),
        atom.before(
          atom.Many([
            background.color(background.ColorExpression(third)),
            background.color(background.ColorExpression(fourth)),
          ]),
        ),
      ]),
    )
  assert class_names == ["a0", "a1"]
  assert rules
    == [
      context.Rule("a0", ".a0{background-color:rgb(0% 0% 2%)}"),
      context.Rule("a1", ".a1::before{background-color:rgb(0% 0% 4%)}"),
    ]

  let #(_, rules, class_names) =
    atom.compile(
      context.styling(),
      atom.Many([
        fill.fill(fill.Expression(first_paint)),
        background.color(background.ColorExpression(second)),
      ]),
    )
  assert class_names == ["a0", "a1"]
  assert rules
    == [
      context.Rule("a0", ".a0{fill:rgb(0% 0% 1%)}"),
      context.Rule("a1", ".a1{background-color:rgb(0% 0% 2%)}"),
    ]
}

// Mirrors the positive media fixtures in the original media test source. The
// source's extra-rules rejection remains in the oracle until the
// Gleam compiler has an equivalent error assertion.
fn media_surface() {
  let scroll = typed_expr.keyword("scroll")
  assert_scoped(
    overflow.x(
      overflow.Expression(media.match_typed(
        media.All,
        media.reduced_motion(),
        scroll,
      )),
    ),
    ".a0{@media only all and (prefers-reduced-motion: reduce){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(media.match_typed(
        media.Screen,
        media.min_width(values.LengthPercentageLength(data.px(300.0))),
        scroll,
      )),
    ),
    ".a0{@media only screen and (min-width: 300px){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(media.match_typed(
        media.Screen,
        media.min_width(
          values.LengthPercentageLengthExpression(
            typed_expr.add(typed_expr.length(data.rem(30.0)), [
              typed_expr.length(data.px(300.0)),
            ]),
          ),
        ),
        scroll,
      )),
    ),
    ".a0{@media only screen and (min-width: calc(30rem + 300px)){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(media.match_typed(media.All, media.hover(), scroll)),
    ),
    ".a0{@media only all and (hover: hover){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(media.match_typed(
        media.Screen,
        media.and(media.reduced_motion_value(media.Reduce), [
          media.orientation(media.Portrait),
        ]),
        scroll,
      )),
    ),
    ".a0{@media only screen and ((prefers-reduced-motion: reduce) and (orientation: portrait)){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(media.match_typed(
        media.Screen,
        media.not(media.reduced_motion_value(media.Reduce), [
          media.orientation(media.Portrait),
        ]),
        scroll,
      )),
    ),
    ".a0{@media only screen and (not ((prefers-reduced-motion: reduce) and (orientation: portrait))){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(media.match_typed(
        media.Screen,
        media.or(media.reduced_motion_value(media.Reduce), [
          media.orientation(media.Portrait),
        ]),
        scroll,
      )),
    ),
    ".a0{@media only screen and ((prefers-reduced-motion: reduce) or (orientation: portrait)){overflow-x:scroll}}",
  )
}

// Mirrors the positive container fixtures in the original styling test source. A built
// container is passed through the opaque value returned by the real
// constructor, including the source's stable-name and named-scope behavior.
fn container_surface() {
  let #(styling, first) = container_dsl.build(context.styling())
  let #(styling, second) = container_dsl.build(styling)
  let style =
    atom.Many([
      container_dsl.name(container_dsl.value(first), [
        container_dsl.value(second),
      ]),
      overflow.x(
        overflow.Expression(container_dsl.match_named_typed(
          first,
          container_dsl.orientation(container_dsl.Portrait),
          typed_expr.keyword("scroll"),
        )),
      ),
    ])
  let #(styling, rules, class_names) = atom.compile(styling, style)
  assert class_names == ["a0", "a1"]
  assert rules
    == [
      context.Rule("a0", ".a0{container-name:r0 r1}"),
      context.Rule(
        "a1",
        ".a1{@container r0 (orientation: portrait){overflow-x:scroll}}",
      ),
    ]
  let #(_, repeated_rules, repeated_class_names) = atom.compile(styling, style)
  assert repeated_class_names == class_names
  assert repeated_rules == rules

  assert_single(container_dsl.type_(container_dsl.Size), "container-type:size")
  assert_scoped(
    overflow.x(
      overflow.Expression(container_dsl.match_all_typed(
        container_dsl.orientation(container_dsl.Portrait),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@container (orientation: portrait){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(container_dsl.match_all_typed(
        container_dsl.min_width(values.LengthPercentageLength(data.px(300.0))),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@container (min-width: 300px){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(container_dsl.match_all_typed(
        container_dsl.min_width(
          values.LengthPercentageLengthExpression(
            typed_expr.add(typed_expr.length(data.rem(30.0)), [
              typed_expr.length(data.px(300.0)),
            ]),
          ),
        ),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@container (min-width: calc(30rem + 300px)){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(container_dsl.match_all_typed(
        container_dsl.and(
          container_dsl.min_block_size(
            values.LengthPercentageLength(data.px(400.0)),
          ),
          [container_dsl.orientation(container_dsl.Portrait)],
        ),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@container ((min-block-size: 400px) and (orientation: portrait)){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(container_dsl.match_all_typed(
        container_dsl.not(
          container_dsl.min_block_size(
            values.LengthPercentageLength(data.px(400.0)),
          ),
          [container_dsl.orientation(container_dsl.Portrait)],
        ),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@container (not ((min-block-size: 400px) and (orientation: portrait))){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(container_dsl.match_all_typed(
        container_dsl.or(
          container_dsl.min_block_size(
            values.LengthPercentageLength(data.px(400.0)),
          ),
          [container_dsl.orientation(container_dsl.Portrait)],
        ),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@container ((min-block-size: 400px) or (orientation: portrait)){overflow-x:scroll}}",
  )
}

// Mirrors the positive before pseudo-element fixtures in
// the original pseudo-element test source. The conflicting before-after case
// stays in the oracle until expected panics are represented explicitly.
fn pseudo_surface() {
  assert_scoped(
    atom.before(
      background.color(
        background.ColorExpression(typed_expr.rgb(
          typed_expr.pct(data.pct(100.0)),
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(50.0)),
        )),
      ),
    ),
    ".a0::before{background-color:rgb(100% 0% 50%)}",
  )
  let value =
    typed_expr.cascade(
      typed_expr.rgb(
        typed_expr.pct(data.pct(100.0)),
        typed_expr.pct(data.pct(0.0)),
        typed_expr.pct(data.pct(49.0)),
      ),
      [
        select.match_typed(
          select.hover(),
          typed_expr.rgb(
            typed_expr.pct(data.pct(100.0)),
            typed_expr.pct(data.pct(0.0)),
            typed_expr.pct(data.pct(50.0)),
          ),
        ),
      ],
    )
  let #(_, rules, class_names) =
    atom.compile(
      context.styling(),
      atom.before(background.color(background.ColorExpression(value))),
    )
  assert class_names == ["e0", "a0"]
  assert rules
    == [
      context.Rule(
        "e0",
        ".e0{--e0:rgb(100% 0% 49%);&:where(:hover){--e0:rgb(100% 0% 50%)}}",
      ),
      context.Rule("a0", ".a0::before{background-color:var(--e0)}"),
    ]
}

// Mirrors the reuse and nested-cascade fixtures in
// the original expression-interning fixtures. These cases exercise the observable
// custom-property extraction, scope order, and repeated expression identity.
fn expression_surface() {
  let nested =
    typed_expr.cascade(
      typed_expr.rgb(
        typed_expr.pct(data.pct(0.0)),
        typed_expr.pct(data.pct(0.0)),
        typed_expr.pct(data.pct(0.0)),
      ),
      [
        typed_expr.cascade(
          typed_expr.rgb(
            typed_expr.pct(data.pct(0.0)),
            typed_expr.pct(data.pct(0.0)),
            typed_expr.pct(data.pct(1.0)),
          ),
          [
            typed_expr.rgb(
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(2.0)),
            ),
          ],
        ),
        typed_expr.rgb(
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(3.0)),
        ),
        select.match_typed(
          select.hover(),
          typed_expr.cascade(
            typed_expr.rgb(
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(4.0)),
            ),
            [
              typed_expr.cascade(
                typed_expr.rgb(
                  typed_expr.pct(data.pct(0.0)),
                  typed_expr.pct(data.pct(0.0)),
                  typed_expr.pct(data.pct(5.0)),
                ),
                [
                  typed_expr.rgb(
                    typed_expr.pct(data.pct(0.0)),
                    typed_expr.pct(data.pct(0.0)),
                    typed_expr.pct(data.pct(6.0)),
                  ),
                ],
              ),
              typed_expr.rgb(
                typed_expr.pct(data.pct(0.0)),
                typed_expr.pct(data.pct(0.0)),
                typed_expr.pct(data.pct(7.0)),
              ),
            ],
          ),
        ),
        typed_expr.rgb(
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(8.0)),
        ),
        typed_expr.rgb(
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(9.0)),
        ),
      ],
    )
  let #(_, nested_rules, nested_class_names) =
    atom.compile(
      context.styling(),
      background.color(background.ColorExpression(nested)),
    )
  assert nested_class_names == ["a0"]
  assert nested_rules
    == [
      context.Rule(
        "a0",
        ".a0{background-color:rgb(0% 0% 0%);background-color:rgb(0% 0% 1%);background-color:rgb(0% 0% 2%);background-color:rgb(0% 0% 3%);&:where(:hover){background-color:rgb(0% 0% 4%);background-color:rgb(0% 0% 5%);background-color:rgb(0% 0% 6%);background-color:rgb(0% 0% 7%)}&{background-color:rgb(0% 0% 8%);background-color:rgb(0% 0% 9%)}}",
      ),
    ]

  let scoped_value =
    typed_expr.cascade(typed_expr.pct(data.pct(0.0)), [
      select.match_typed(select.hover(), typed_expr.pct(data.pct(1.0))),
    ])
  let style =
    atom.atom(
      "background-color",
      typed_expr.erase(typed_expr.rgb(
        typed_expr.pct(data.pct(0.0)),
        scoped_value,
        scoped_value,
      )),
    )
  let #(_, rules, class_names) = atom.compile(context.styling(), style)
  assert class_names == ["e0", "a0"]
  assert rules
    == [
      context.Rule("e0", ".e0{--e0:0%;&:where(:hover){--e0:1%}}"),
      context.Rule("a0", ".a0{background-color:rgb(0% var(--e0) var(--e0))}"),
    ]

  let chained =
    typed_expr.cascade(typed_expr.pct(data.pct(0.0)), [
      select.match_typed(
        select.hover(),
        typed_expr.add(typed_expr.pct(data.pct(1.0)), [
          typed_expr.cascade(typed_expr.pct(data.pct(0.0)), [
            select.match_typed(select.disabled(), typed_expr.pct(data.pct(2.0))),
          ]),
        ]),
      ),
    ])
  let #(_, rules, class_names) =
    atom.compile(
      context.styling(),
      atom.atom(
        "background-color",
        typed_expr.erase(typed_expr.rgb(
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.0)),
          chained,
        )),
      ),
    )
  assert class_names == ["e0", "e1", "a0"]
  assert rules
    == [
      context.Rule("e0", ".e0{--e0:0%;&:where(:disabled){--e0:2%}}"),
      context.Rule(
        "e1",
        ".e1{--e1:0%;&:where(:hover){--e1:calc(1% + var(--e0))}}",
      ),
      context.Rule("a0", ".a0{background-color:rgb(0% 0% var(--e1))}"),
    ]
}

// Mirrors the direct selector fixtures in the original selector test source. The
// nested `:where` forms are intentionally asserted as emitted by the compiler
// rather than normalized by a formatter.
fn select_surface() {
  let value =
    typed_expr.rgb(
      typed_expr.pct(data.pct(100.0)),
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(50.0)),
    )
  assert_scoped(
    background.color(
      background.ColorExpression(select.match_typed(select.hover(), value)),
    ),
    ".a0{&:where(:hover){background-color:rgb(100% 0% 50%)}}",
  )
  assert_scoped(
    background.color(
      background.ColorExpression(select.match_typed(
        select.dir(select.Ltr),
        value,
      )),
    ),
    ".a0{&:where(:dir(ltr)){background-color:rgb(100% 0% 50%)}}",
  )
  assert_scoped(
    background.color(
      background.ColorExpression(select.match_typed(
        select.dir(select.Rtl),
        value,
      )),
    ),
    ".a0{&:where(:dir(rtl)){background-color:rgb(100% 0% 50%)}}",
  )
  assert_scoped(
    background.color(
      background.ColorExpression(select.match_typed(
        select.and(select.disabled(), [select.hover()]),
        value,
      )),
    ),
    ".a0{&:where(:disabled:hover){background-color:rgb(100% 0% 50%)}}",
  )
  assert_scoped(
    background.color(
      background.ColorExpression(select.match_typed(
        select.not(select.disabled(), [select.hover()]),
        value,
      )),
    ),
    ".a0{&:where(:not(:disabled:hover)){background-color:rgb(100% 0% 50%)}}",
  )
  assert_scoped(
    background.color(
      background.ColorExpression(select.match_typed(
        select.or(select.disabled(), [select.hover()]),
        value,
      )),
    ),
    ".a0{&:where(:where(:disabled,:hover)){background-color:rgb(100% 0% 50%)}}",
  )
  assert_scoped(
    background.color(
      background.ColorExpression(select.match_typed(
        select.and(
          select.or(select.disabled(), [
            select.and(select.empty(), [select.hover()]),
          ]),
          [select.not(select.first_child(), [select.last_child()])],
        ),
        value,
      )),
    ),
    ".a0{&:where(:where(:disabled,:empty:hover):not(:first-child:last-child)){background-color:rgb(100% 0% 50%)}}",
  )
}

// Mirrors the positive support-rule fixtures in the original support test source.
fn support_surface() {
  let auto_term = support.code(hyphen.mode(hyphen.Auto))
  let manual_term = support.code(hyphen.mode(hyphen.Manual))
  let value = support.match_typed(auto_term, typed_expr.keyword("scroll"))
  assert_scoped(
    overflow.x(overflow.Expression(value)),
    ".a0{@supports (hyphens:auto){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(support.match_typed(
        support.code(
          atom.Many([
            hyphen.mode(hyphen.Auto),
            hyphen.mode(hyphen.Manual),
          ]),
        ),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@supports ((hyphens:auto) and (hyphens:manual)){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(support.match_typed(
        support.and(auto_term, [manual_term]),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@supports ((hyphens:auto) and (hyphens:manual)){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(support.match_typed(
        support.not(auto_term, [manual_term]),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@supports (not ((hyphens:auto) and (hyphens:manual))){overflow-x:scroll}}",
  )
  assert_scoped(
    overflow.x(
      overflow.Expression(support.match_typed(
        support.or(auto_term, [manual_term]),
        typed_expr.keyword("scroll"),
      )),
    ),
    ".a0{@supports ((hyphens:auto) or (hyphens:manual)){overflow-x:scroll}}",
  )
}

fn support_identity() {
  let first =
    support.match(
      support.and(support.code(atom.atom("hyphens", expr.keyword("auto"))), [
        support.code(atom.atom("overflow-wrap", expr.keyword("normal"))),
      ]),
      expr.keyword("scroll"),
    )
  let second =
    support.match(
      support.and(support.code(atom.atom("hyphens", expr.keyword("manual"))), [
        support.code(atom.atom("overflow-wrap", expr.keyword("normal"))),
      ]),
      expr.keyword("scroll"),
    )
  assert expr.expression_key(first) != expr.expression_key(second)
  let #(_, rules, _) =
    atom.compile(
      context.styling(),
      atom.Many([atom.atom("first", first), atom.atom("second", second)]),
    )
  assert rules
    == [
      context.Rule(
        "a0",
        ".a0{@supports ((hyphens:auto) and (overflow-wrap:normal)){first:scroll}}",
      ),
      context.Rule(
        "a1",
        ".a1{@supports ((hyphens:manual) and (overflow-wrap:normal)){second:scroll}}",
      ),
    ]
}

// Mirrors the basic property fixtures in the original property test sources. The
// expected strings are the exact compact rule bodies emitted before the
// test helper formats them with Prettier.
fn property_surface() {
  let typed_length = typed_expr.length(data.px(1.0))
  let typed_percentage = typed_expr.pct(data.pct(1.0))
  let typed_source_rgb =
    typed_expr.rgb(
      typed_expr.pct(data.pct(100.0)),
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(50.0)),
    )

  assert_single(accent.color(accent.Auto), "accent-color:auto")
  assert_single(
    accent.color(accent.Expression(typed_source_rgb)),
    "accent-color:rgb(100% 0% 50%)",
  )
  assert_single(
    background.color(background.ColorExpression(typed_source_rgb)),
    "background-color:rgb(100% 0% 50%)",
  )
  assert_single(
    background.image(
      gradient.linear(
        gradient.Angle(data.deg(1.0)),
        gradient.value(
          gradient.ColorExpression(typed_expr.rgb(
            typed_expr.pct(data.pct(0.0)),
            typed_expr.pct(data.pct(0.0)),
            typed_expr.pct(data.pct(2.0)),
          )),
        ),
        [
          gradient.plain(
            gradient.value(
              gradient.ColorExpression(typed_expr.rgb(
                typed_expr.pct(data.pct(0.0)),
                typed_expr.pct(data.pct(0.0)),
                typed_expr.pct(data.pct(3.0)),
              )),
            ),
          ),
        ],
      ),
      [
        gradient.linear_stops(
          gradient.AngleExpression(typed_expr.angle(data.deg(4.0))),
          gradient.value(
            gradient.ColorExpression(typed_expr.rgb(
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(0.0)),
              typed_expr.pct(data.pct(5.0)),
            )),
          ),
          [
            gradient.hint(
              gradient.PercentageExpression(typed_expr.pct(data.pct(6.0))),
              gradient.stop(
                gradient.ColorExpression(typed_expr.rgb(
                  typed_expr.pct(data.pct(0.0)),
                  typed_expr.pct(data.pct(0.0)),
                  typed_expr.pct(data.pct(7.0)),
                )),
                gradient.PercentageExpression(typed_expr.pct(data.pct(8.0))),
                Some(
                  gradient.PercentageExpression(typed_expr.pct(data.pct(9.0))),
                ),
              ),
            ),
          ],
        ),
      ],
    ),
    "background-image:linear-gradient(1deg,rgb(0% 0% 2%),rgb(0% 0% 3%)),linear-gradient(4deg,rgb(0% 0% 5%),6%,rgb(0% 0% 7%) 8% 9%)",
  )
  assert_single(
    background.size(background.Single(background.Cover), [
      background.Pair(
        background.LengthExpression(typed_expr.length(data.px(1.0))),
        background.Auto,
      ),
    ]),
    "background-size:cover,1px auto",
  )
  assert_many(border_style.oi(border_style.Solid), [
    "border-block-start-style:solid",
    "border-block-end-style:solid",
    "border-inline-start-style:solid",
    "border-inline-end-style:solid",
  ])
  assert_single(border_style.l(border_style.Dashed), "border-left-style:dashed")
  assert_many(
    border_width.oi(
      values.LengthOnlyExpression(typed_expr.length(data.px(1.0))),
    ),
    [
      "border-block-start-width:1px",
      "border-block-end-width:1px",
      "border-inline-start-width:1px",
      "border-inline-end-width:1px",
    ],
  )
  assert_single(caret.color(caret.Auto), "caret-color:auto")
  assert_single(
    caret.color(caret.Expression(typed_source_rgb)),
    "caret-color:rgb(100% 0% 50%)",
  )
  assert_single(
    content_dsl.content(content_dsl.String(data.string_literal("hello")), []),
    "content:\"hello\"",
  )
  assert_single(
    content_dsl.content(content_dsl.String(data.string_literal(" (")), [
      content_dsl.Expression(typed_expr.css_string(data.attr("href"))),
      content_dsl.String(data.string_literal(")")),
    ]),
    "content:\" (\" attr(href) \")\"",
  )
  assert_single(fill.fill(fill.Paint(data.ContextFill)), "fill:context-fill")
  assert_single(font.size(font.SizeLength(data.px(1.0))), "font-size:1px")
  assert_single(font.size(font.SizePercentage(data.pct(75.0))), "font-size:75%")
  assert_single(font.size(font.SizeKeyword(font.Large)), "font-size:large")
  assert_single(
    font.size(font.SizeLengthExpression(typed_length)),
    "font-size:1px",
  )
  assert_single(
    font.size(font.SizeKeywordExpression(typed_expr.keyword("large"))),
    "font-size:large",
  )
  assert_single(font.weight(font.WeightNumber(1.0)), "font-weight:1")
  assert_single(font.weight(font.WeightKeyword(font.Bold)), "font-weight:bold")
  assert_single(
    font.weight(font.WeightNumberExpression(typed_expr.number(1.0))),
    "font-weight:1",
  )
  assert_single(
    font_variant.ligatures(
      font.ConfiguredLigatures(font.LigaturesCommon(
        font.CommonLigatures,
        None,
        None,
        None,
      )),
    ),
    "font-variant-ligatures:common-ligatures",
  )
  assert_single(
    font_variant.ligatures(
      font.ConfiguredLigatures(font.LigaturesCommon(
        font.CommonLigaturesExpression(typed_expr.keyword("common-ligatures")),
        None,
        None,
        None,
      )),
    ),
    "font-variant-ligatures:common-ligatures",
  )
  assert_single(
    font_variant.ligatures(font.NormalLigatures),
    "font-variant-ligatures:normal",
  )
  assert_single(
    font_variant.ligatures(
      font.ConfiguredLigatures(font.LigaturesCommon(
        font.CommonLigatures,
        Some(font.DiscretionaryLigatures),
        Some(font.HistoricalLigatures),
        Some(font.ContextualLigatures),
      )),
    ),
    "font-variant-ligatures:common-ligatures discretionary-ligatures historical-ligatures contextual-ligatures",
  )
  assert_single(
    font_variant.numeric(
      font.ConfiguredNumeric(font.NumericLining(
        font.OldstyleNums,
        Some(font.TabularNums),
        Some(font.StackedFractions),
        Some(font.Ordinal),
        Some(font.SlashedZero),
      )),
    ),
    "font-variant-numeric:oldstyle-nums tabular-nums stacked-fractions ordinal slashed-zero",
  )
  assert_single(
    font_variant.numeric(font.NormalNumeric),
    "font-variant-numeric:normal",
  )
  assert_single(hyphen.mode(hyphen.Auto), "hyphens:auto")
  assert_many(inset.oi(values.LengthPercentageLengthExpression(typed_length)), [
    "inset-block-start:1px",
    "inset-block-end:1px",
    "inset-inline-start:1px",
    "inset-inline-end:1px",
  ])
  assert_single(
    inset.l(values.LengthPercentageLengthExpression(typed_length)),
    "left:1px",
  )
  assert_single(isolation.isolation(isolation.Isolate), "isolation:isolate")
  assert_single(line.height(values.LineHeightNumber(1.0)), "line-height:1")
  assert_single(
    line.height(values.LineHeightLengthExpression(typed_length)),
    "line-height:1px",
  )
  assert_many(margin.oi(values.LengthPercentageAutoValue), [
    "margin-block-start:auto",
    "margin-block-end:auto",
    "margin-inline-start:auto",
    "margin-inline-end:auto",
  ])
  assert_single(
    margin.l(values.LengthPercentageAutoLengthExpression(typed_length)),
    "margin-left:1px",
  )
  assert_many(overflow.xy(overflow.Scroll), [
    "overflow-x:scroll",
    "overflow-y:scroll",
  ])
  assert_many(
    padding.oi(values.LengthPercentageLengthExpression(typed_length)),
    [
      "padding-block-start:1px",
      "padding-block-end:1px",
      "padding-inline-start:1px",
      "padding-inline-end:1px",
    ],
  )
  assert_single(
    padding.l(values.LengthPercentageLengthExpression(typed_length)),
    "padding-left:1px",
  )
  assert_single(pointer.events(pointer.None), "pointer-events:none")
  assert_single(position.position(position.Fixed), "position:fixed")
  assert_single(
    punctuation.hang(
      Some(punctuation.ValuesFirst(
        punctuation.First,
        None,
        Some(punctuation.Last),
      )),
    ),
    "hanging-punctuation:first last",
  )
  assert_single(punctuation.hang(None), "hanging-punctuation:none")
  assert_single(size.mode(values.BoxSizingBorderBox), "box-sizing:border-box")
  assert_many(size.min_wh(values.LengthPercentageLength(data.px(1.0))), [
    "min-width:1px",
    "min-height:1px",
  ])
  assert_many(size.max_wh(values.LengthPercentagePct(data.pct(1.0))), [
    "max-width:1%",
    "max-height:1%",
  ])
  assert_many(size.wh(values.LengthPercentageLength(data.px(1.0))), [
    "width:1px",
    "height:1px",
  ])
  assert_single(
    size.min_o(values.LengthPercentageLength(data.px(1.0))),
    "min-block-size:1px",
  )
  assert_single(
    size.min_i(values.LengthPercentageLength(data.px(1.0))),
    "min-inline-size:1px",
  )
  assert_many(size.min_oi(values.LengthPercentageLength(data.px(1.0))), [
    "min-block-size:1px",
    "min-inline-size:1px",
  ])
  assert_single(
    size.max_o(values.LengthPercentageLength(data.px(1.0))),
    "max-block-size:1px",
  )
  assert_single(
    size.max_i(values.LengthPercentageLength(data.px(1.0))),
    "max-inline-size:1px",
  )
  assert_many(size.max_oi(values.LengthPercentageLength(data.px(1.0))), [
    "max-block-size:1px",
    "max-inline-size:1px",
  ])
  assert_single(
    size.o(values.LengthPercentageLength(data.px(1.0))),
    "block-size:1px",
  )
  assert_single(
    size.i(values.LengthPercentageLength(data.px(1.0))),
    "inline-size:1px",
  )
  assert_many(size.oi(values.LengthPercentageLength(data.px(1.0))), [
    "block-size:1px",
    "inline-size:1px",
  ])
  assert_single(
    size.mode(values.BoxSizingExpression(typed_expr.keyword("border-box"))),
    "box-sizing:border-box",
  )
  assert_single(
    size.min_height(values.LengthPercentageLengthExpression(typed_length)),
    "min-height:1px",
  )
  assert_single(
    size.max_height(values.LengthPercentagePctExpression(typed_percentage)),
    "max-height:1%",
  )
  assert_single(
    size.height(values.LengthPercentageLengthExpression(typed_length)),
    "height:1px",
  )
  assert_single(styling_text.wrap(styling_text.Pretty), "text-wrap:pretty")
}

// Mirrors the source math fixtures. The Gleam API makes the expression
// value explicit, but the emitted calc/min/max/clamp forms remain source-
// compatible.
fn math_surface() {
  let percent = expr.pct(data.pct(1.0))
  let two_percent = expr.pct(data.pct(2.0))
  let three_percent = expr.pct(data.pct(3.0))
  assert_single(
    atom.atom("--test", expr.add(percent, [two_percent, three_percent])),
    "--test:calc(1% + 2% + 3%)",
  )
  assert_single(
    atom.atom("--test", expr.sub(percent, [two_percent, three_percent])),
    "--test:calc(1% - 2% - 3%)",
  )
  assert_single(
    atom.atom("--test", expr.mul(percent, [expr.number(2.0), expr.number(3.0)])),
    "--test:calc(1% * 2 * 3)",
  )
  assert_single(
    atom.atom("--test", expr.div(percent, [expr.number(2.0), expr.number(3.0)])),
    "--test:calc(1% / 2 / 3)",
  )
  assert_single(
    atom.atom("--test", expr.min(percent, [two_percent, three_percent])),
    "--test:min(1%,2%,3%)",
  )
  assert_single(
    atom.atom("--test", expr.max(percent, [two_percent, three_percent])),
    "--test:max(1%,2%,3%)",
  )
  assert_single(
    atom.atom("--test", expr.clamp(percent, two_percent, three_percent)),
    "--test:clamp(1%,2%,3%)",
  )
  assert_variable(
    expr.add(expr.number(1.0), [expr.number(2.0), expr.number(3.0)]),
    "--s0:calc(1 + 2 + 3)",
  )
  assert_variable(
    expr.clamp(expr.number(1.0), expr.number(2.0), expr.number(3.0)),
    "--s0:clamp(1,2,3)",
  )
  assert_variable(
    expr.div(expr.number(1.0), [expr.number(2.0), expr.number(3.0)]),
    "--s0:calc(1 / 2 / 3)",
  )
  assert_variable(
    expr.min(expr.number(1.0), [expr.number(2.0), expr.number(3.0)]),
    "--s0:min(1,2,3)",
  )
  assert_variable(
    expr.max(expr.number(1.0), [expr.number(2.0), expr.number(3.0)]),
    "--s0:max(1,2,3)",
  )
  assert_variable(
    expr.mul(expr.number(1.0), [expr.number(2.0), expr.number(3.0)]),
    "--s0:calc(1 * 2 * 3)",
  )
}

// generatedName.buildIdentifier uses Number.toString(16), so the first index
// after `f` is `10`, not decimal `16`. Keep this source-backed boundary test
// separate from the small fixture helpers above, whose indices stay below 10.
fn hexadecimal_generated_names() {
  let style = unique_atoms(17, 0, [])
  let #(_context, rules, class_names) = atom.compile(context.styling(), style)
  assert list.drop(class_names, 15) == ["af", "a10"]
  assert list.drop(list.map(rules, context.rule_class_name), 15)
    == ["af", "a10"]
  assert list.drop(list.map(rules, context.rule_code), 15)
    == [".af{property-15:x}", ".a10{property-16:x}"]
}

fn unique_atoms(
  count: Int,
  index: Int,
  output: List(atom.AtomOpt),
) -> atom.AtomOpt {
  case count {
    0 -> atom.Many(list.reverse(output))
    _ ->
      unique_atoms(count - 1, index + 1, [
        atom.atom("property-" <> int.to_string(index), expr.keyword("x")),
        ..output
      ])
  }
}

fn assert_single(value: atom.AtomOpt, expected: String) {
  let #(_, rules, class_names) = atom.compile(context.styling(), value)
  assert class_names == ["a0"]
  assert list.map(rules, context.rule_code) == [".a0{" <> expected <> "}"]
}

fn assert_scoped(value: atom.AtomOpt, expected: String) {
  let #(_, rules, class_names) = atom.compile(context.styling(), value)
  assert class_names == ["a0"]
  assert list.map(rules, context.rule_code) == [expected]
}

fn assert_variable(value: expr.Expression, expected: String) {
  let #(context, variable) = var_dsl.build(context.styling())
  let #(_, rules, class_names) =
    atom.compile(context, var_dsl.set(variable, value))
  assert class_names == ["a0"]
  assert list.map(rules, context.rule_code) == [".a0{" <> expected <> "}"]
}

fn assert_many(value: atom.AtomOpt, expected: List(String)) {
  let #(_, rules, class_names) = atom.compile(context.styling(), value)
  assert class_names == expected_class_names(expected, 0)
  assert list.map(rules, context.rule_code) == expected_rules(expected, 0)
}

fn expected_class_names(values: List(String), index: Int) -> List(String) {
  case values {
    [] -> []
    [_, ..rest] -> [
      "a" <> int.to_string(index),
      ..expected_class_names(rest, index + 1)
    ]
  }
}

fn expected_rules(values: List(String), index: Int) -> List(String) {
  case values {
    [] -> []
    [first, ..rest] -> [
      ".a" <> int.to_string(index) <> "{" <> first <> "}",
      ..expected_rules(rest, index + 1)
    ]
  }
}

fn variable_identity() {
  let #(context, first) = var_dsl.build(context.styling())
  let #(context, second) = var_dsl.build(context)
  let style =
    atom.Many([
      var_dsl.set_typed(first, typed_expr.pct(data.pct(1.0))),
      var_dsl.set_typed(second, typed_expr.pct(data.pct(2.0))),
      background.color(
        background.ColorExpression(typed_expr.rgb(
          typed_expr.pct(data.pct(0.0)),
          var_dsl.get_typed(first),
          var_dsl.get_typed(second),
        )),
      ),
    ])
  let #(context, rules, class_names) = atom.compile(context, style)
  assert class_names == ["a0", "a1", "a2"]
  assert rules
    == [
      context.Rule("a0", ".a0{--s0:1%}"),
      context.Rule("a1", ".a1{--s1:2%}"),
      context.Rule("a2", ".a2{background-color:rgb(0% var(--s0) var(--s1))}"),
    ]
  let #(_context, repeated_rules, repeated_class_names) =
    atom.compile(context, style)
  assert repeated_class_names == ["a0", "a1", "a2"]
  assert repeated_rules == rules
  let #(context, first_name) = var_dsl.resolve(context, first)
  let #(_context, second_name) = var_dsl.resolve(context, second)
  assert first_name == "--s0"
  assert second_name == "--s1"

  // Source `var_()` assigns each instance a unique Symbol, even when two
  // variables are created from the same Context snapshot. The Gleam identity
  // seam must preserve that distinction for atom and expression cache keys.
  let base = context.styling()
  let #(first_context, first) = var_dsl.build(base)
  let #(_, second) = var_dsl.build(base)
  let style =
    atom.Many([
      var_dsl.set_typed(first, typed_expr.pct(data.pct(1.0))),
      var_dsl.set_typed(second, typed_expr.pct(data.pct(1.0))),
      background.color(
        background.ColorExpression(typed_expr.rgb(
          typed_expr.pct(data.pct(0.0)),
          var_dsl.get_typed(first),
          var_dsl.get_typed(second),
        )),
      ),
    ])
  let #(_, rules, class_names) = atom.compile(first_context, style)
  assert class_names == ["a0", "a1", "a2"]
  assert rules
    == [
      context.Rule("a0", ".a0{--s0:1%}"),
      context.Rule("a1", ".a1{--s1:1%}"),
      context.Rule("a2", ".a2{background-color:rgb(0% var(--s0) var(--s1))}"),
    ]

  let #(context, variable) = var_dsl.build(context.styling())
  let fallback =
    typed_expr.cascade(typed_expr.pct(data.pct(0.0)), [
      select.match_typed(select.hover(), typed_expr.pct(data.pct(1.0))),
    ])
  let #(_context, rules, class_names) =
    atom.compile(
      context,
      background.color(
        background.ColorExpression(typed_expr.rgb(
          typed_expr.pct(data.pct(0.0)),
          typed_expr.pct(data.pct(0.0)),
          var_dsl.or_typed(variable, fallback),
        )),
      ),
    )
  assert class_names == ["e0", "a0"]
  assert rules
    == [
      context.Rule("e0", ".e0{--e0:0%;&:where(:hover){--e0:1%}}"),
      context.Rule(
        "a0",
        ".a0{background-color:rgb(0% 0% var(--s0, var(--e0)))}",
      ),
    ]

  let #(context, variable) = var_dsl.build(context.styling())
  let #(_context, rules, _) =
    atom.compile(
      context,
      atom.atom(
        "overflow-x",
        media.match(
          media.Screen,
          media.min_width(
            values.LengthPercentageLengthExpression(var_dsl.get_typed(variable)),
          ),
          expr.keyword("scroll"),
        ),
      ),
    )
  assert rules
    == [
      context.Rule(
        "a0",
        ".a0{@media only screen and (min-width: var(--s0)){overflow-x:scroll}}",
      ),
    ]

  let #(context, variable) = var_dsl.build(context.styling())
  let condition = support.code(var_dsl.set(variable, expr.pct(data.pct(1.0))))
  let #(_context, rules, _) =
    atom.compile(
      context,
      atom.atom("color", support.match(condition, expr.keyword("red"))),
    )
  assert rules
    == [
      context.Rule("a0", ".a0{@supports (--s0:1%){color:red}}"),
    ]
}

fn container_identity() {
  // `container.build()` also creates a source Symbol independently of the
  // styling context. Two containers made from the same context snapshot must
  // therefore retain distinct generated names inside nested expressions.
  let base = context.styling()
  let #(first_context, first) = container_dsl.build(base)
  let #(_, second) = container_dsl.build(base)
  let condition = container_dsl.orientation(container_dsl.Portrait)
  let first_value =
    typed_expr.rgb(
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(0.0)),
      container_dsl.match_named_typed(
        first,
        condition,
        typed_expr.pct(data.pct(1.0)),
      ),
    )
  let second_value =
    typed_expr.rgb(
      typed_expr.pct(data.pct(0.0)),
      typed_expr.pct(data.pct(0.0)),
      container_dsl.match_named_typed(
        second,
        condition,
        typed_expr.pct(data.pct(1.0)),
      ),
    )
  let style =
    atom.Many([
      atom.atom("first", typed_expr.erase(first_value)),
      atom.atom("second", typed_expr.erase(second_value)),
    ])
  let #(_, rules, class_names) = atom.compile(first_context, style)
  assert class_names == ["e0", "a0", "e1", "a1"]
  assert rules
    == [
      context.Rule("e0", ".e0{@container r0 (orientation: portrait){--e0:1%}}"),
      context.Rule("a0", ".a0{first:rgb(0% 0% var(--e0))}"),
      context.Rule("e1", ".e1{@container r1 (orientation: portrait){--e1:1%}}"),
      context.Rule("a1", ".a1{second:rgb(0% 0% var(--e1))}"),
    ]
}

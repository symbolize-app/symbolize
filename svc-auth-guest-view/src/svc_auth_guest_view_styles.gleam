import gleam/option
import lib_dataflow as dataflow
import lib_markup_dom as dom
import lib_markup_fragment as fragment
import lib_markup_html as html
import lib_markup_style as markup_style
import lib_styling_accent as accent
import lib_styling_atom as atom
import lib_styling_background as background
import lib_styling_caret as caret
import lib_styling_content as content
import lib_styling_data as styling_data
import lib_styling_font as font
import lib_styling_font_variant as font_variant
import lib_styling_gradient as gradient
import lib_styling_hyphen as hyphen
import lib_styling_inset as inset
import lib_styling_isolation as isolation
import lib_styling_overflow as overflow
import lib_styling_padding as padding
import lib_styling_pointer as pointer
import lib_styling_position as position
import lib_styling_pseudo as pseudo
import lib_styling_punctuation as punctuation
import lib_styling_size as size
import lib_styling_text as text
import lib_styling_typed_expr as typed_expr
import lib_styling_values as styling_values
import lib_styling_var as styling_var

pub fn html_style(document: dom.Document) -> fragment.Fragment {
  html.portal(
    dom.document_element(document),
    html.DivAttrs(
      ..html.div_attrs(),
      style: style([
        accent.color(
          accent.Expression(typed_expr.rgb(
            typed_expr.pct(styling_data.pct(100.0)),
            typed_expr.pct(styling_data.pct(0.0)),
            typed_expr.pct(styling_data.pct(100.0)),
          )),
        ),
        caret.color(
          caret.Expression(typed_expr.rgb(
            typed_expr.pct(styling_data.pct(100.0)),
            typed_expr.pct(styling_data.pct(0.0)),
            typed_expr.pct(styling_data.pct(100.0)),
          )),
        ),
        font_variant.ligatures(
          font.ConfiguredLigatures(font.LigaturesCommon(
            font.CommonLigatures,
            option.Some(font.DiscretionaryLigatures),
            option.None,
            option.None,
          )),
        ),
        font_variant.numeric(
          font.ConfiguredNumeric(font.NumericLining(
            font.OldstyleNums,
            option.None,
            option.None,
            option.None,
            option.None,
          )),
        ),
        hyphen.mode(hyphen.Auto),
        overflow.wrap(overflow.BreakWord),
        punctuation.hang(
          option.Some(punctuation.ValuesFirst(
            punctuation.First,
            option.None,
            option.Some(punctuation.Last),
          )),
        ),
        text.wrap(text.Pretty),
      ]),
    ),
  )
}

pub fn body_style(
  host: markup_style.Host,
  document: dom.Document,
) -> #(markup_style.Host, fragment.Fragment) {
  let #(host, grid_color) = markup_style.variable(host)
  body_style_with_variable(host, document, grid_color)
}

pub fn body_style_with_variable(
  host: markup_style.Host,
  document: dom.Document,
  grid_color: styling_var.Var(styling_data.Color),
) -> #(markup_style.Host, fragment.Fragment) {
  let transparent =
    typed_expr.rgb_alpha(
      typed_expr.pct(styling_data.pct(0.0)),
      typed_expr.pct(styling_data.pct(0.0)),
      typed_expr.pct(styling_data.pct(0.0)),
      typed_expr.pct(styling_data.pct(0.0)),
    )
  let grid = styling_var.get_typed(grid_color)
  let grid_rule =
    styling_var.set_typed(
      grid_color,
      typed_expr.light_dark(
        typed_expr.hsl_alpha(
          typed_expr.angle(styling_data.deg(240.0)),
          typed_expr.pct(styling_data.pct(100.0)),
          typed_expr.pct(styling_data.pct(36.0)),
          typed_expr.pct(styling_data.pct(5.0)),
        ),
        typed_expr.hsl_alpha(
          typed_expr.angle(styling_data.deg(240.0)),
          typed_expr.pct(styling_data.pct(100.0)),
          typed_expr.pct(styling_data.pct(70.0)),
          typed_expr.pct(styling_data.pct(20.0)),
        ),
      ),
    )
  let grid_x =
    gradient.linear(
      gradient.Angle(styling_data.deg(0.0)),
      gradient.stop(
        gradient.ColorExpression(grid),
        gradient.Length(styling_data.px(1.0)),
        option.None,
      ),
      [
        gradient.plain(gradient.stop(
          gradient.ColorExpression(transparent),
          gradient.Length(styling_data.px(1.0)),
          option.None,
        )),
      ],
    )
  let grid_y =
    gradient.linear(
      gradient.Angle(styling_data.deg(90.0)),
      gradient.stop(
        gradient.ColorExpression(grid),
        gradient.Length(styling_data.px(1.0)),
        option.None,
      ),
      [
        gradient.plain(gradient.stop(
          gradient.ColorExpression(transparent),
          gradient.Length(styling_data.px(1.0)),
          option.None,
        )),
      ],
    )
  let rules =
    pseudo.after(
      atom.Many([
        grid_rule,
        background.image(grid_x, [grid_y]),
        background.size(
          background.Pair(
            background.LengthExpression(
              typed_expr.length(styling_data.rlh(0.5)),
            ),
            background.LengthExpression(
              typed_expr.length(styling_data.rlh(0.5)),
            ),
          ),
          [],
        ),
        content.content(
          content.Expression(
            typed_expr.css_string(styling_data.string_literal("")),
          ),
          [],
        ),
        inset.oi(
          styling_values.LengthPercentageLengthExpression(
            typed_expr.length(styling_data.rlh(0.0)),
          ),
        ),
        isolation.isolation(isolation.Isolate),
        pointer.events(pointer.None),
        position.position(position.Absolute),
      ]),
    )
  let direct_rules =
    atom.Many([
      overflow.x(overflow.Hidden),
      position.position(position.Relative),
      padding.outer_inner(
        styling_values.LengthPercentageLengthExpression(
          typed_expr.length(styling_data.rlh(2.0)),
        ),
      ),
      size.min_height(
        styling_values.LengthPercentageLengthExpression(
          typed_expr.length(styling_data.length(100.0, styling_data.Dvh)),
        ),
      ),
    ])
  let style = style([direct_rules, rules])
  let fragment =
    html.portal(
      dom.body(document),
      html.DivAttrs(..html.div_attrs(), style: style),
    )
  #(host, fragment)
}

fn style(values: List(atom.AtomOpt)) -> dataflow.NodeOpt(atom.AtomOpt) {
  dataflow.literal(atom.Many(values))
}

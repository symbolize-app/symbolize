import gleam/int
import gleam/option.{Some}
import lib_dataflow as dataflow
import lib_markup_attributes as markup_attributes
import lib_markup_custom as markup_custom
import lib_markup_data as svg_data
import lib_markup_dom as dom
import lib_markup_fragment as fragment
import lib_markup_html as html
import lib_markup_math as math
import lib_markup_select as markup_select
import lib_markup_svg as svg
import lib_styling_atom as atom
import lib_styling_background as background
import lib_styling_border_color as border_color
import lib_styling_border_style as border_style
import lib_styling_border_width as border_width
import lib_styling_container as container
import lib_styling_data as styling_data
import lib_styling_fill as fill
import lib_styling_font as font
import lib_styling_line as line
import lib_styling_margin as margin
import lib_styling_media as media
import lib_styling_select as select
import lib_styling_size as size
import lib_styling_support as support
import lib_styling_typed_expr as typed_expr
import lib_styling_values as styling_values
import lib_styling_var as styling_var

pub fn custom(
  head: dom.Node,
  fill_var: styling_var.Var(styling_data.SvgPaint),
  extra: container.Container,
  title: dataflow.NodeOpt(String),
) -> fragment.Fragment {
  let view =
    markup_custom.define(fn(scoped, attrs: CustomAttrs) { build(scoped, attrs) })
  view(CustomAttrs(head: head, fill_var: fill_var, extra: extra, title: title))
}

type CustomAttrs {
  CustomAttrs(
    head: dom.Node,
    fill_var: styling_var.Var(styling_data.SvgPaint),
    extra: container.Container,
    title: dataflow.NodeOpt(String),
  )
}

fn build(
  scoped: fragment.ScopedContext,
  attrs: CustomAttrs,
) -> fragment.FragmentInput {
  let context = fragment.scoped_dataflow(scoped)
  let CustomAttrs(head, fill_var, extra, title) = attrs
  let count = dataflow.state(0)
  let count_style =
    dataflow.reactive(dataflow.map(count_style, dataflow.mutation(count)))
  let count_text =
    dataflow.map2(
      fn(title, count) { title <> " / " <> int.to_string(count) },
      title,
      dataflow.mutation(count),
    )
  let on_count_click =
    dataflow.handler(
      fn(_event, current) {
        let assert Ok(Nil) =
          dataflow.txn(context, fn() {
            dataflow.set(context, count, current + 1)
          })
        Nil
      },
      dataflow.mutation(count),
    )
  let title_element =
    html.title(
      html.DivAttrs(
        ..html.div_attrs(),
        on_add: Some(
          fragment.sync_on_add(fn(_scoped) { dom.remove_first_title(head) }),
        ),
        content: fragment.TextInput("Symbolize Custom"),
      ),
    )
  let fill_expression = styling_var.get_typed(fill_var)
  let select_condition =
    select.or(select.empty(), [
      select.and(select.not(select.disabled(), []), [
        select.hover(),
        markup_select.attr(
          markup_select.SelectAttrs(
            ..markup_select.attrs(),
            display: Some(markup_select.DisplayBlock),
            nonce: Some("x"),
          ),
        ),
        markup_select.type_(markup_select.Math, [markup_select.Svg]),
      ]),
    ])
  let math_background =
    background.color(
      background.ColorExpression(typed_expr.rgb(
        typed_expr.cascade(typed_expr.pct(styling_data.pct(0.0)), [
          media.match_typed(
            media.All,
            media.min_width(
              styling_values.LengthPercentageLength(styling_data.px(400.0)),
            ),
            typed_expr.pct(styling_data.pct(100.0)),
          ),
        ]),
        typed_expr.pct(styling_data.pct(75.0)),
        typed_expr.cascade(typed_expr.pct(styling_data.pct(100.0)), [
          select.match_typed(
            select_condition,
            typed_expr.pct(styling_data.pct(0.0)),
          ),
        ]),
      )),
    )
  fragment.ListInput([
    fragment.FragmentInput(html.portal(
      head,
      html.DivAttrs(
        ..html.div_attrs(),
        content: fragment.FragmentInput(title_element),
      ),
    )),
    fragment.FragmentInput(html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        content: fragment.ListInput([
          fragment.FragmentInput(html.h1(
            html.DivAttrs(
              ..html.div_attrs(),
              style: style([
                font.size(
                  font.SizeLengthExpression(
                    typed_expr.length(styling_data.rem(2.0)),
                  ),
                ),
                font.weight(font.WeightNumber(700.0)),
                line.height(
                  styling_values.LineHeightLengthExpression(
                    typed_expr.length(styling_data.rlh(2.0)),
                  ),
                ),
                margin.oe(
                  styling_values.LengthPercentageAutoLengthExpression(
                    typed_expr.length(styling_data.rlh(1.0)),
                  ),
                ),
                size.max_width(
                  styling_values.LengthPercentageLengthExpression(
                    typed_expr.length(styling_data.length(
                      60.0,
                      styling_data.Rch,
                    )),
                  ),
                ),
              ]),
              content: fragment.TextInput(
                "The Tale of Peter Rabbit by Beatrix Potter",
              ),
            ),
          )),
          fragment.FragmentInput(html.h2(
            html.DivAttrs(
              ..html.div_attrs(),
              style: style([
                font.size(
                  font.SizeLength(styling_data.length(1.6, styling_data.Rem)),
                ),
                font.weight(font.WeightNumber(500.0)),
                line.height(
                  styling_values.LineHeightLengthExpression(
                    typed_expr.length(styling_data.rlh(1.5)),
                  ),
                ),
                margin.oe(
                  styling_values.LengthPercentageAutoLengthExpression(
                    typed_expr.length(styling_data.rlh(0.5)),
                  ),
                ),
                size.max_width(
                  styling_values.LengthPercentageLengthExpression(
                    typed_expr.length(styling_data.length(
                      60.0,
                      styling_data.Rch,
                    )),
                  ),
                ),
              ]),
              content: fragment.TextInput("Chapter 1"),
            ),
          )),
          fragment.FragmentInput(html.p(
            html.DivAttrs(
              ..html.div_attrs(),
              style: style([
                margin.oe(
                  styling_values.LengthPercentageAutoLengthExpression(
                    typed_expr.length(styling_data.rlh(0.5)),
                  ),
                ),
                size.max_width(
                  styling_values.LengthPercentageLengthExpression(
                    typed_expr.length(styling_data.length(
                      60.0,
                      styling_data.Rch,
                    )),
                  ),
                ),
              ]),
              content: fragment.TextInput(
                "Once upon a time there were four little Rabbits, and their names were— Flopsy, Mopsy, Cotton-tail, and Peter.",
              ),
            ),
          )),
          fragment.FragmentInput(html.p(
            html.DivAttrs(
              ..html.div_attrs(),
              style: style([
                margin.oe(
                  styling_values.LengthPercentageAutoLengthExpression(
                    typed_expr.length(styling_data.rlh(0.5)),
                  ),
                ),
                size.max_width(
                  styling_values.LengthPercentageLengthExpression(
                    typed_expr.length(styling_data.length(
                      60.0,
                      styling_data.Rch,
                    )),
                  ),
                ),
              ]),
              content: fragment.TextInput(
                "They lived with their Mother in a sand-bank, underneath the root of a very big fir-tree.",
              ),
            ),
          )),
          fragment.FragmentInput(html.p(
            html.DivAttrs(
              ..html.div_attrs(),
              style: style([
                size.max_width(
                  styling_values.LengthPercentageLengthExpression(
                    typed_expr.length(styling_data.length(
                      60.0,
                      styling_data.Rch,
                    )),
                  ),
                ),
              ]),
              content: fragment.TextInput(
                "'Now, my dears,' said old Mrs. Rabbit one morning, 'you may go into the fields or down the lane, but don't go into Mr. McGregor's garden.'",
              ),
            ),
          )),
        ]),
      ),
    )),
    fragment.FragmentInput(html.hr(
      html.DivAttrs(
        ..html.div_attrs(),
        style: style([
          border_style.oi(border_style.None),
          border_color.oe(
            border_color.Expression(typed_expr.light_dark(
              typed_expr.rgb(
                typed_expr.pct(styling_data.pct(0.0)),
                typed_expr.pct(styling_data.pct(0.0)),
                typed_expr.pct(styling_data.pct(0.0)),
              ),
              typed_expr.rgb(
                typed_expr.pct(styling_data.pct(100.0)),
                typed_expr.pct(styling_data.pct(100.0)),
                typed_expr.pct(styling_data.pct(100.0)),
              ),
            )),
          ),
          border_style.oe(border_style.Solid),
          border_width.oe(
            styling_values.LengthOnlyExpression(
              typed_expr.length(styling_data.px(1.0)),
            ),
          ),
          margin.oe(
            styling_values.LengthPercentageAutoLengthExpression(
              typed_expr.sub(typed_expr.length(styling_data.rlh(3.0)), [
                typed_expr.length(styling_data.px(1.0)),
              ]),
            ),
          ),
          margin.os(
            styling_values.LengthPercentageAutoLengthExpression(
              typed_expr.length(styling_data.rlh(3.0)),
            ),
          ),
        ]),
      ),
    )),
    fragment.FragmentInput(html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        style: count_style,
        events: markup_attributes.Events(
          ..markup_attributes.events(),
          on_click: Some(markup_attributes.sync_listener(on_count_click)),
        ),
        content: fragment.ReactiveTextInput(dataflow.reactive(count_text)),
      ),
    )),
    fragment.FragmentInput(svg.svg(
      svg.SvgAttrs(
        ..svg.svg_attrs(),
        style: style([
          fill.fill(
            fill.Expression(
              typed_expr.cascade(
                typed_expr.rgb_paint(
                  typed_expr.mul(typed_expr.pct(styling_data.pct(30.0)), [
                    typed_expr.number(2.0),
                  ]),
                  typed_expr.div(typed_expr.pct(styling_data.pct(120.0)), [
                    typed_expr.number(2.0),
                  ]),
                  typed_expr.max(typed_expr.pct(styling_data.pct(0.0)), [
                    typed_expr.pct(styling_data.pct(100.0)),
                  ]),
                ),
                [
                  select.match_typed(
                    select.hover(),
                    typed_expr.rgb_paint(
                      typed_expr.pct(styling_data.pct(0.0)),
                      typed_expr.pct(styling_data.pct(0.0)),
                      typed_expr.pct(styling_data.pct(100.0)),
                    ),
                  ),
                ],
              ),
            ),
          ),
        ]),
        height: dataflow.literal(Some(svg_length(50.0))),
        view_box: dataflow.literal(Some(svg_data_rect())),
        width: dataflow.literal(Some(svg_length(50.0))),
        content: fragment.FragmentInput(svg.rect(
          svg.RectAttrs(
            ..svg.rect_attrs(),
            style: style([fill.fill(fill.Expression(fill_expression))]),
            height: dataflow.literal(Some(svg_length(80.0))),
            width: dataflow.literal(Some(svg_length(80.0))),
            x: dataflow.literal(Some(svg_length(10.0))),
            y: dataflow.literal(Some(svg_length(10.0))),
          ),
        )),
      ),
    )),
    fragment.FragmentInput(math.math(
      math.MathAttrs(
        ..math.math_attrs(),
        style: style([math_background]),
        display: dataflow.literal(Some(math.MathBlock)),
        nonce: dataflow.literal(Some("x")),
        content: fragment.ListInput([
          fragment.FragmentInput(math.mi(
            math.MiAttrs(..math.mi_attrs(), content: fragment.TextInput("x")),
          )),
          fragment.FragmentInput(math.mo(
            math.MoAttrs(..math.mo_attrs(), content: fragment.TextInput("+")),
          )),
          fragment.FragmentInput(math.mi(
            math.MiAttrs(..math.mi_attrs(), content: fragment.TextInput("y")),
          )),
        ]),
      ),
    )),
    fragment.FragmentInput(html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        style: style([
          container.name(container.value(extra), []),
          container.type_(container.InlineSize),
        ]),
        content: fragment.ListInput([
          fragment.FragmentInput(
            html.input(html.checkbox(
              html.CheckboxAttrs(
                ..html.checkbox_attrs(),
                checked: dataflow.literal(Some(True)),
              ),
            )),
          ),
          fragment.FragmentInput(
            html.input(html.text_input(
              html.TextInputAttrs(
                ..html.text_input_attrs(),
                style: style([
                  background.color(
                    background.ColorExpression(container.match_named_typed(
                      extra,
                      container.min_inline_size(
                        styling_values.LengthPercentageLength(styling_data.rlh(
                          30.0,
                        )),
                      ),
                      typed_expr.rgb_alpha(
                        typed_expr.pct(styling_data.pct(100.0)),
                        typed_expr.pct(styling_data.pct(0.0)),
                        typed_expr.pct(styling_data.pct(0.0)),
                        typed_expr.pct(styling_data.pct(5.0)),
                      ),
                    )),
                  ),
                ]),
                value: dataflow.literal(Some("abc")),
              ),
            )),
          ),
        ]),
      ),
    )),
  ])
}

fn count_style(count: Int) -> atom.AtomOpt {
  case count % 2 {
    1 ->
      background.color(
        background.ColorExpression(typed_expr.rgb_alpha(
          typed_expr.pct(styling_data.pct(80.0)),
          typed_expr.pct(styling_data.pct(80.0)),
          typed_expr.pct(styling_data.pct(80.0)),
          typed_expr.cascade(typed_expr.pct(styling_data.pct(0.0)), [
            support.match_typed(
              support.and(
                support.code(
                  size.outer_inner(
                    styling_values.LengthPercentageLengthExpression(
                      typed_expr.length(styling_data.px(2.0)),
                    ),
                  ),
                ),
                [
                  support.not(
                    support.code(
                      size.outer_inner(
                        styling_values.LengthPercentageLengthExpression(
                          typed_expr.length(styling_data.px(-2.0)),
                        ),
                      ),
                    ),
                    [],
                  ),
                ],
              ),
              typed_expr.pct(styling_data.pct(100.0)),
            ),
            support.match_typed(
              support.code(
                size.outer_inner(
                  styling_values.LengthPercentageLengthExpression(
                    typed_expr.length(styling_data.px(-2.0)),
                  ),
                ),
              ),
              typed_expr.pct(styling_data.pct(50.0)),
            ),
          ]),
        )),
      )
    _ -> atom.Empty
  }
}

fn style(values: List(atom.AtomOpt)) -> dataflow.NodeOpt(atom.AtomOpt) {
  dataflow.literal(atom.Many(values))
}

fn svg_length(value: Float) -> svg_data.SvgLengthPctOpt {
  svg_data.SvgNumber(value)
}

fn svg_data_rect() -> svg_data.Rect {
  svg_data.rect(svg_data.RectAttrs(
    left: 0.0,
    top: 0.0,
    width: 100.0,
    height: 100.0,
  ))
}

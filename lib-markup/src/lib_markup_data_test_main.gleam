import gleam/io
import gleam/option.{None, Some}
import lib_markup_data as data
import lib_markup_fragment as fragment
import lib_markup_html as html
import lib_markup_math as math
import lib_markup_range as markup_range
import lib_markup_select as markup_select
import lib_styling_atom as atom
import lib_styling_context as styling_context
import lib_styling_data as styling_data
import lib_styling_expr as styling_expr
import lib_styling_select as styling_select

pub fn run(done: fn() -> Nil) {
  assert data.svg_preserve_aspect_ratio_text(
      data.svg_preserve_aspect_ratio(data.SvgPreserveAspectRatioAttrs(
        align: data.XMaxYMax,
        mode: Some(data.Slice),
      )),
    )
    == "xMaxYMax slice"
  assert data.svg_preserve_aspect_ratio_text(
      data.svg_preserve_aspect_ratio(data.SvgPreserveAspectRatioAttrs(
        align: data.NoneAlign,
        mode: None,
      )),
    )
    == "none"
  assert data.rect(data.RectAttrs(left: 1.0, top: 2.0, width: 3.0, height: 4.0))
    == #(1.0, 2.0, 3.0, 4.0)
  assert data.svg_length_text(data.svg_px(2.5)) == "2.5px"
  assert data.svg_length_text(data.svg_length(3.0, data.SvgEm)) == "3em"
  assert fragment.text_content(fragment.to_fragment(fragment.TextInput("text")))
    == "text"
  assert fragment.text_content(fragment.to_fragment(fragment.EmptyInput)) == ""
  assert fragment.text_content(
      fragment.to_fragment(
        fragment.ListInput([
          fragment.TextInput("a"),
          fragment.ListInput([fragment.TextInput("b"), fragment.EmptyInput]),
          fragment.FragmentInput(fragment.text("c")),
        ]),
      ),
    )
    == "abc"
  assert fragment.text_content(
      markup_range.range(
        markup_range.RangeAttrs(content: [
          fragment.TextInput("a"),
          fragment.TextInput("b"),
        ]),
      ),
    )
    == "ab"
  let attrs =
    html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("x"))
  assert fragment.text_content(html.div(attrs)) == "x"
  selector_parity()
  io.println("lib-markup data Gleam parity tests passed")
  done()
}

pub fn main() {
  run(fn() { Nil })
}

fn selector_parity() {
  let isolated =
    atom.atom(
      "isolation",
      styling_select.match(
        markup_select.attr(
          markup_select.SelectAttrs(
            ..markup_select.attrs(),
            access_key: Some("x"),
          ),
        ),
        styling_expr.keyword("isolate"),
      ),
    )
  let #(_context, rules, names) =
    atom.compile(styling_context.styling(), isolated)
  assert names == ["a0"]
  assert rules
    == [
      styling_context.Rule(
        "a0",
        ".a0{&:where([accesskey=\"x\"]){isolation:isolate}}",
      ),
    ]

  let escaped =
    atom.atom(
      "isolation",
      styling_select.match(
        markup_select.attr(
          markup_select.SelectAttrs(
            ..markup_select.attrs(),
            access_key: Some("'\"\\,\n"),
          ),
        ),
        styling_expr.keyword("isolate"),
      ),
    )
  let #(_context, rules, _) = atom.compile(styling_context.styling(), escaped)
  assert rules
    == [
      styling_context.Rule(
        "a0",
        ".a0{&:where([accesskey=\"'\\\"\\\\,\\n\"]){isolation:isolate}}",
      ),
    ]

  let #(_context, rules, _) =
    atom.compile(
      styling_context.styling(),
      atom.atom(
        "isolation",
        styling_select.match(
          markup_select.attr(
            markup_select.SelectAttrs(
              ..markup_select.attrs(),
              inert: Some(True),
            ),
          ),
          styling_expr.keyword("isolate"),
        ),
      ),
    )
  assert rules
    == [
      styling_context.Rule("a0", ".a0{&:where([inert]){isolation:isolate}}"),
    ]

  let #(_context, rules, _) =
    atom.compile(
      styling_context.styling(),
      atom.atom(
        "isolation",
        styling_select.match(
          markup_select.attr(
            markup_select.SelectAttrs(
              ..markup_select.attrs(),
              inert: Some(False),
            ),
          ),
          styling_expr.keyword("isolate"),
        ),
      ),
    )
  assert rules
    == [
      styling_context.Rule(
        "a0",
        ".a0{&:where(:not([inert])){isolation:isolate}}",
      ),
    ]

  let #(_context, rules, _) =
    atom.compile(
      styling_context.styling(),
      atom.atom(
        "isolation",
        styling_select.match(
          markup_select.attr(
            markup_select.SelectAttrs(
              ..markup_select.attrs(),
              access_key: Some("x"),
              id: Some("y"),
            ),
          ),
          styling_expr.keyword("isolate"),
        ),
      ),
    )
  assert rules
    == [
      styling_context.Rule(
        "a0",
        ".a0{&:where([accesskey=\"x\"][id=\"y\"]){isolation:isolate}}",
      ),
    ]

  let complete_map =
    atom.atom(
      "isolation",
      styling_select.match(
        markup_select.attr(
          markup_select.SelectAttrs(
            ..markup_select.attrs(),
            aria_checked: Some(html.AriaMixed),
            autofocus: Some(False),
            class_names: Some(["x", "y"]),
            height: Some(data.SvgNumber(2.0)),
            hidden: Some(html.HiddenBoolean),
            l_space: Some(math.MathPercentage(styling_data.pct(5.0))),
            type_: Some("submit"),
          ),
        ),
        styling_expr.keyword("isolate"),
      ),
    )
  let #(_context, rules, _) =
    atom.compile(styling_context.styling(), complete_map)
  assert rules
    == [
      styling_context.Rule(
        "a0",
        ".a0{&:where([aria-checked=\"mixed\"]:not([autofocus])[class=\"x y\"][height=\"2\"][hidden][lspace=\"5%\"][type=\"submit\"]){isolation:isolate}}",
      ),
    ]

  let #(_context, rules, _) =
    atom.compile(
      styling_context.styling(),
      atom.atom(
        "isolation",
        styling_select.match(
          markup_select.type_(markup_select.Button, [markup_select.G]),
          styling_expr.keyword("isolate"),
        ),
      ),
    )
  assert rules
    == [
      styling_context.Rule(
        "a0",
        ".a0{&:where(:where(button, g)){isolation:isolate}}",
      ),
    ]
  io.println("lib-markup selector Gleam parity passed")
}

import gleam/float
import gleam/string

pub opaque type Length {
  Length(value: Float, unit: LengthUnit)
}

pub type LengthUnit {
  Cap
  Ch
  Cm
  Dvb
  Dvh
  Dvi
  Dvmax
  Dvmin
  Dvw
  Em
  Ex
  Ic
  In
  Lh
  Lvb
  Lvh
  Lvi
  Lvmax
  Lvmin
  Lvw
  Mm
  Pc
  Pt
  Px
  Q
  Rcap
  Rch
  Rem
  Rex
  Ric
  Rlh
  Svb
  Svh
  Svi
  Svmax
  Svmin
  Svw
  Vb
  Vh
  Vi
  Vmax
  Vmin
  Vw
}

pub fn length(value: Float, unit: LengthUnit) -> Length {
  Length(value, unit)
}

pub fn em(value: Float) -> Length {
  length(value, Em)
}

pub fn lh(value: Float) -> Length {
  length(value, Lh)
}

pub fn pt(value: Float) -> Length {
  length(value, Pt)
}

pub fn px(value: Float) -> Length {
  length(value, Px)
}

pub fn rem(value: Float) -> Length {
  length(value, Rem)
}

pub fn rlh(value: Float) -> Length {
  length(value, Rlh)
}

pub fn length_text(value: Length) -> String {
  case value {
    Length(number, unit) -> css_number_text(number) <> length_unit_text(unit)
  }
}

pub fn number_text(value: Float) -> String {
  css_number_text(value)
}

fn length_unit_text(unit: LengthUnit) -> String {
  case unit {
    Cap -> "cap"
    Ch -> "ch"
    Cm -> "cm"
    Dvb -> "dvb"
    Dvh -> "dvh"
    Dvi -> "dvi"
    Dvmax -> "dvmax"
    Dvmin -> "dvmin"
    Dvw -> "dvw"
    Em -> "em"
    Ex -> "ex"
    Ic -> "ic"
    In -> "in"
    Lh -> "lh"
    Lvb -> "lvb"
    Lvh -> "lvh"
    Lvi -> "lvi"
    Lvmax -> "lvmax"
    Lvmin -> "lvmin"
    Lvw -> "lvw"
    Mm -> "mm"
    Pc -> "pc"
    Pt -> "pt"
    Px -> "px"
    Q -> "Q"
    Rcap -> "rcap"
    Rch -> "rch"
    Rem -> "rem"
    Rex -> "rex"
    Ric -> "ric"
    Rlh -> "rlh"
    Svb -> "svb"
    Svh -> "svh"
    Svi -> "svi"
    Svmax -> "svmax"
    Svmin -> "svmin"
    Svw -> "svw"
    Vb -> "vb"
    Vh -> "vh"
    Vi -> "vi"
    Vmax -> "vmax"
    Vmin -> "vmin"
    Vw -> "vw"
  }
}

pub opaque type Pct {
  Pct(Float)
}

pub fn pct(value: Float) -> Pct {
  Pct(value)
}

pub fn percentage_text(value: Pct) -> String {
  case value {
    Pct(number) -> css_number_text(number) <> "%"
  }
}

pub opaque type Angle {
  Angle(value: Float, unit: AngleUnit)
}

pub type AngleUnit {
  Deg
  Grad
  Rad
  Turn
}

pub fn angle(value: Float, unit: AngleUnit) -> Angle {
  Angle(value, unit)
}

pub fn deg(value: Float) -> Angle {
  angle(value, Deg)
}

pub fn angle_text(value: Angle) -> String {
  case value {
    Angle(number, unit) -> css_number_text(number) <> angle_unit_text(unit)
  }
}

fn angle_unit_text(unit: AngleUnit) -> String {
  case unit {
    Deg -> "deg"
    Grad -> "grad"
    Rad -> "rad"
    Turn -> "turn"
  }
}

pub opaque type CssString {
  StringLiteral(String)
  Attr(String)
}

pub fn string_literal(value: String) -> CssString {
  StringLiteral(value)
}

pub fn attr(name: String) -> CssString {
  Attr(name)
}

pub fn string_text(value: CssString) -> String {
  case value {
    StringLiteral(text) -> quote(text)
    Attr(name) -> "attr(" <> name <> ")"
  }
}

fn quote(value: String) -> String {
  "\"" <> escape_string(string.to_utf_codepoints(value), "") <> "\""
}

fn escape_string(values: List(UtfCodepoint), output: String) -> String {
  case values {
    [] -> output
    [first, ..rest] -> escape_string(rest, output <> escape_codepoint(first))
  }
}

fn escape_codepoint(value: UtfCodepoint) -> String {
  let codepoint = string.utf_codepoint_to_int(value)
  case codepoint {
    8 -> "\\b"
    9 -> "\\t"
    10 -> "\\n"
    12 -> "\\f"
    13 -> "\\r"
    34 -> "\\\""
    92 -> "\\\\"
    _ ->
      case codepoint < 0x20 {
        True -> "\\u" <> hex4(codepoint)
        False -> string.from_utf_codepoints([value])
      }
  }
}

fn hex4(value: Int) -> String {
  hex_digit(value / 0x1000)
  <> hex_digit(value / 0x100 % 16)
  <> hex_digit(value / 16 % 16)
  <> hex_digit(value % 16)
}

fn hex_digit(value: Int) -> String {
  case value {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    15 -> "f"
    _ -> panic as "invalid hexadecimal digit"
  }
}

fn css_number_text(value: Float) -> String {
  let raw = float.to_string(value)
  case raw {
    "//js(NaN)" -> "NaN"
    "//js(Infinity)" -> "Infinity"
    "//js(-Infinity)" -> "-Infinity"
    _ ->
      case string.split_once(raw, on: "e") {
        Error(_) -> trim_float_suffix(raw)
        Ok(#(mantissa, exponent)) -> {
          let sign = case string.starts_with(exponent, "-") {
            True -> ""
            False -> "+"
          }
          trim_float_suffix(mantissa) <> "e" <> sign <> exponent
        }
      }
  }
}

fn trim_float_suffix(value: String) -> String {
  case string.ends_with(value, ".0") {
    True -> string.drop_end(from: value, up_to: 2)
    False -> value
  }
}

pub opaque type Color {
  Color(String)
}

pub fn color_text(value: Color) -> String {
  case value {
    Color(text) -> text
  }
}

pub opaque type Gradient {
  Gradient(String)
}

pub fn gradient_text(value: Gradient) -> String {
  case value {
    Gradient(text) -> text
  }
}

pub type Image =
  Gradient

pub type SvgPaint {
  PaintColor(Color)
  ContextFill
  ContextStroke
  NoPaint
}

pub fn svg_paint_text(value: SvgPaint) -> String {
  case value {
    PaintColor(color) -> color_text(color)
    ContextFill -> "context-fill"
    ContextStroke -> "context-stroke"
    NoPaint -> "none"
  }
}

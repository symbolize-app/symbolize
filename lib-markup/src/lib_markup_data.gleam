import gleam/option.{type Option, None, Some}
import lib_styling_data as styling

pub type PreserveAlign {
  NoneAlign
  XMaxYMax
  XMaxYMid
  XMaxYMin
  XMidYMax
  XMidYMid
  XMidYMin
  XMinYMax
  XMinYMid
  XMinYMin
}

pub type PreserveMode {
  Meet
  Slice
}

pub type SvgPreserveAspectRatio {
  SvgPreserveAspectRatio(align: PreserveAlign, mode: PreserveMode)
}

pub type SvgPreserveAspectRatioOpt {
  Align(PreserveAlign)
  WithMode(SvgPreserveAspectRatio)
}

pub type SvgPreserveAspectRatioAttrs {
  SvgPreserveAspectRatioAttrs(align: PreserveAlign, mode: Option(PreserveMode))
}

pub fn svg_preserve_aspect_ratio(
  attrs: SvgPreserveAspectRatioAttrs,
) -> SvgPreserveAspectRatioOpt {
  let SvgPreserveAspectRatioAttrs(align, mode) = attrs
  case mode {
    None -> Align(align)
    Some(mode) -> WithMode(SvgPreserveAspectRatio(align: align, mode: mode))
  }
}

pub fn svg_preserve_aspect_ratio_text(
  value: SvgPreserveAspectRatioOpt,
) -> String {
  case value {
    Align(align) -> preserve_align_text(align)
    WithMode(SvgPreserveAspectRatio(align, mode)) ->
      preserve_align_text(align) <> " " <> preserve_mode_text(mode)
  }
}

pub opaque type SvgLength {
  SvgLength(styling.Length)
}

pub type SvgLengthUnit {
  SvgCm
  SvgEm
  SvgEx
  SvgIn
  SvgMm
  SvgPc
  SvgPt
  SvgPx
}

pub fn svg_length(value: Float, unit: SvgLengthUnit) -> SvgLength {
  SvgLength(styling.length(value, styling_unit(unit)))
}

pub fn svg_cm(value: Float) -> SvgLength {
  svg_length(value, SvgCm)
}

pub fn svg_em(value: Float) -> SvgLength {
  svg_length(value, SvgEm)
}

pub fn svg_ex(value: Float) -> SvgLength {
  svg_length(value, SvgEx)
}

pub fn svg_in(value: Float) -> SvgLength {
  svg_length(value, SvgIn)
}

pub fn svg_mm(value: Float) -> SvgLength {
  svg_length(value, SvgMm)
}

pub fn svg_pc(value: Float) -> SvgLength {
  svg_length(value, SvgPc)
}

pub fn svg_pt(value: Float) -> SvgLength {
  svg_length(value, SvgPt)
}

pub fn svg_px(value: Float) -> SvgLength {
  svg_length(value, SvgPx)
}

pub fn svg_length_text(value: SvgLength) -> String {
  let SvgLength(value) = value
  styling.length_text(value)
}

pub type SvgLengthPctOpt {
  SvgPercentage(styling.Pct)
  SvgLengthValue(SvgLength)
  SvgNumber(Float)
}

pub type Rect =
  #(Float, Float, Float, Float)

// The source helper accepts a named record and returns the common tuple used
// by the SVG viewBox attribute. Keep the result type shared with attribute
// handling while making the input field names explicit at call sites.
pub type RectAttrs {
  RectAttrs(left: Float, top: Float, width: Float, height: Float)
}

pub fn rect(attrs: RectAttrs) -> Rect {
  let RectAttrs(left, top, width, height) = attrs
  #(left, top, width, height)
}

fn styling_unit(unit: SvgLengthUnit) -> styling.LengthUnit {
  case unit {
    SvgCm -> styling.Cm
    SvgEm -> styling.Em
    SvgEx -> styling.Ex
    SvgIn -> styling.In
    SvgMm -> styling.Mm
    SvgPc -> styling.Pc
    SvgPt -> styling.Pt
    SvgPx -> styling.Px
  }
}

fn preserve_align_text(value: PreserveAlign) -> String {
  case value {
    NoneAlign -> "none"
    XMaxYMax -> "xMaxYMax"
    XMaxYMid -> "xMaxYMid"
    XMaxYMin -> "xMaxYMin"
    XMidYMax -> "xMidYMax"
    XMidYMid -> "xMidYMid"
    XMidYMin -> "xMidYMin"
    XMinYMax -> "xMinYMax"
    XMinYMid -> "xMinYMid"
    XMinYMin -> "xMinYMin"
  }
}

fn preserve_mode_text(value: PreserveMode) -> String {
  case value {
    Meet -> "meet"
    Slice -> "slice"
  }
}

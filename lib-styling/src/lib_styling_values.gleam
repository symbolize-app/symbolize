import lib_styling_data as data
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

// These are the common value sets shared by the source property modules. The
// property modules choose the narrow set they accept; the explicit expression
// constructor is retained for source-valid scoped/reactive expressions.
pub type LengthPercentage {
  LengthPercentageLength(data.Length)
  LengthPercentagePct(data.Pct)
  LengthPercentageLengthExpression(typed_expr.Expression(data.Length))
  LengthPercentagePctExpression(typed_expr.Expression(data.Pct))
}

pub type LengthOnly {
  LengthOnlyLength(data.Length)
  LengthOnlyExpression(typed_expr.Expression(data.Length))
}

pub type LengthPercentageAuto {
  LengthPercentageAutoLength(data.Length)
  LengthPercentageAutoPct(data.Pct)
  LengthPercentageAutoValue
  LengthPercentageAutoLengthExpression(typed_expr.Expression(data.Length))
  LengthPercentageAutoPctExpression(typed_expr.Expression(data.Pct))
}

pub type LineHeight {
  LineHeightLength(data.Length)
  LineHeightPct(data.Pct)
  LineHeightNumber(Float)
  LineHeightNormal
  LineHeightLengthExpression(typed_expr.Expression(data.Length))
  LineHeightPctExpression(typed_expr.Expression(data.Pct))
  LineHeightNumberExpression(typed_expr.Expression(Float))
  LineHeightNormalExpression(
    typed_expr.Expression(LineHeightNormalKeywordValue),
  )
}

pub type LineHeightNormalKeywordValue {
  LineHeightNormalKeyword
}

pub type BoxSizing {
  BoxSizingBorderBox
  BoxSizingContentBox
  BoxSizingExpression(typed_expr.Expression(BoxSizingKeywordValue))
}

pub type BoxSizingKeywordValue {
  BoxSizingKeyword
}

pub fn length_percentage(value: LengthPercentage) -> expr.Expression {
  case value {
    LengthPercentageLength(value) -> expr.length(value)
    LengthPercentagePct(value) -> expr.pct(value)
    LengthPercentageLengthExpression(value) -> typed_expr.erase(value)
    LengthPercentagePctExpression(value) -> typed_expr.erase(value)
  }
}

pub fn length_only(value: LengthOnly) -> expr.Expression {
  case value {
    LengthOnlyLength(value) -> expr.length(value)
    LengthOnlyExpression(value) -> typed_expr.erase(value)
  }
}

pub fn length_percentage_auto(value: LengthPercentageAuto) -> expr.Expression {
  case value {
    LengthPercentageAutoLength(value) -> expr.length(value)
    LengthPercentageAutoPct(value) -> expr.pct(value)
    LengthPercentageAutoValue -> expr.keyword("auto")
    LengthPercentageAutoLengthExpression(value) -> typed_expr.erase(value)
    LengthPercentageAutoPctExpression(value) -> typed_expr.erase(value)
  }
}

pub fn line_height(value: LineHeight) -> expr.Expression {
  case value {
    LineHeightLength(value) -> expr.length(value)
    LineHeightPct(value) -> expr.pct(value)
    LineHeightNumber(value) -> expr.number(value)
    LineHeightNormal -> expr.keyword("normal")
    LineHeightLengthExpression(value) -> typed_expr.erase(value)
    LineHeightPctExpression(value) -> typed_expr.erase(value)
    LineHeightNumberExpression(value) -> typed_expr.erase(value)
    LineHeightNormalExpression(value) -> typed_expr.erase(value)
  }
}

pub fn box_sizing(value: BoxSizing) -> expr.Expression {
  case value {
    BoxSizingBorderBox -> expr.keyword("border-box")
    BoxSizingContentBox -> expr.keyword("content-box")
    BoxSizingExpression(value) -> typed_expr.erase(value)
  }
}

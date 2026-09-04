import gleam/list
import gleam/option.{type Option, None, Some}
import lib_styling_atom as atom
import lib_styling_data as data
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

// These value types are the finite static portions of the source's
// ExpressionOpt unions. Expression is explicit because a reactive expression
// is opaque to Gleam; the caller is responsible for constructing one whose
// value has the property-specific CSS type.
pub type SizeValue {
  SizeLength(data.Length)
  SizePercentage(data.Pct)
  SizeKeyword(SizeKeyword)
  SizeLengthExpression(typed_expr.Expression(data.Length))
  SizePercentageExpression(typed_expr.Expression(data.Pct))
  SizeKeywordExpression(typed_expr.Expression(SizeKeywordExpressionValue))
}

pub type SizeKeywordExpressionValue {
  SizeKeywordExpressionValueMarker
}

pub type SizeKeyword {
  Large
  Math
  Medium
  Small
  XLarge
  XSmall
  XXLarge
  XXSmall
  XXXLarge
}

pub type WeightValue {
  WeightNumber(Float)
  WeightKeyword(WeightKeyword)
  WeightNumberExpression(typed_expr.Expression(Float))
  WeightKeywordExpression(typed_expr.Expression(WeightKeywordExpressionValue))
}

pub type WeightKeywordExpressionValue {
  WeightKeywordExpressionValueMarker
}

pub type WeightKeyword {
  Bold
  Bolder
  Lighter
  NormalWeight
}

pub type CommonLigature {
  CommonLigatures
  NoCommonLigatures
  CommonLigaturesExpression(
    typed_expr.Expression(CommonLigatureExpressionValue),
  )
}

pub type CommonLigatureExpressionValue {
  CommonLigatureExpression
}

pub type DiscretionaryLigature {
  DiscretionaryLigatures
  NoDiscretionaryLigatures
  DiscretionaryLigaturesExpression(
    typed_expr.Expression(DiscretionaryLigatureExpressionValue),
  )
}

pub type DiscretionaryLigatureExpressionValue {
  DiscretionaryLigatureExpression
}

pub type HistoricalLigature {
  HistoricalLigatures
  NoHistoricalLigatures
  HistoricalLigaturesExpression(
    typed_expr.Expression(HistoricalLigatureExpressionValue),
  )
}

pub type HistoricalLigatureExpressionValue {
  HistoricalLigatureExpression
}

pub type ContextualLigature {
  ContextualLigatures
  NoContextualLigatures
  ContextualLigaturesExpression(
    typed_expr.Expression(ContextualLigatureExpressionValue),
  )
}

pub type ContextualLigatureExpressionValue {
  ContextualLigatureExpression
}

// These variants are the natural Gleam equivalent of the source's ordered,
// non-empty tuple union. The first populated slot determines the constructor;
// later slots remain optional and ordered. This prevents duplicates and keeps
// the source declaration guarantee that an empty argument tuple is invalid.
pub type Ligatures {
  LigaturesCommon(
    CommonLigature,
    Option(DiscretionaryLigature),
    Option(HistoricalLigature),
    Option(ContextualLigature),
  )
  LigaturesDiscretionary(
    DiscretionaryLigature,
    Option(HistoricalLigature),
    Option(ContextualLigature),
  )
  LigaturesHistorical(HistoricalLigature, Option(ContextualLigature))
  LigaturesContextual(ContextualLigature)
}

pub type LigatureValue {
  ConfiguredLigatures(Ligatures)
  NormalLigatures
  NoneLigatures
}

pub type LiningNumbers {
  LiningNums
  OldstyleNums
  LiningNumsExpression(typed_expr.Expression(LiningNumberExpressionValue))
  OldstyleNumsExpression(typed_expr.Expression(LiningNumberExpressionValue))
}

pub type LiningNumberExpressionValue {
  LiningNumberExpression
}

pub type ProportionalNumbers {
  ProportionalNums
  TabularNums
  ProportionalNumsExpression(
    typed_expr.Expression(ProportionalNumberExpressionValue),
  )
  TabularNumsExpression(
    typed_expr.Expression(ProportionalNumberExpressionValue),
  )
}

pub type ProportionalNumberExpressionValue {
  ProportionalNumberExpression
}

pub type FractionNumbers {
  DiagonalFractions
  StackedFractions
  DiagonalFractionsExpression(
    typed_expr.Expression(FractionNumberExpressionValue),
  )
  StackedFractionsExpression(
    typed_expr.Expression(FractionNumberExpressionValue),
  )
}

pub type FractionNumberExpressionValue {
  FractionNumberExpression
}

pub type OrdinalNumber {
  Ordinal
  OrdinalExpression(typed_expr.Expression(OrdinalExpressionValue))
}

pub type OrdinalExpressionValue {
  OrdinalExpressionValueMarker
}

pub type SlashedZeroNumber {
  SlashedZero
  SlashedZeroExpression(typed_expr.Expression(SlashedZeroExpressionValue))
}

pub type SlashedZeroExpressionValue {
  SlashedZeroExpressionValueMarker
}

pub type Numeric {
  NumericLining(
    LiningNumbers,
    Option(ProportionalNumbers),
    Option(FractionNumbers),
    Option(OrdinalNumber),
    Option(SlashedZeroNumber),
  )
  NumericProportional(
    ProportionalNumbers,
    Option(FractionNumbers),
    Option(OrdinalNumber),
    Option(SlashedZeroNumber),
  )
  NumericFractions(
    FractionNumbers,
    Option(OrdinalNumber),
    Option(SlashedZeroNumber),
  )
  NumericOrdinal(OrdinalNumber, Option(SlashedZeroNumber))
  NumericSlashedZero(SlashedZeroNumber)
}

pub type NumericValue {
  ConfiguredNumeric(Numeric)
  NormalNumeric
}

pub fn size(value: SizeValue) -> atom.AtomOpt {
  atom.atom("font-size", size_expression(value))
}

pub fn weight(value: WeightValue) -> atom.AtomOpt {
  atom.atom("font-weight", weight_expression(value))
}

pub fn variant_ligatures(value: LigatureValue) -> atom.AtomOpt {
  let expression = case value {
    NormalLigatures -> expr.keyword("normal")
    NoneLigatures -> expr.keyword("none")
    ConfiguredLigatures(value) -> expr.function("", " ", ligature_values(value))
  }
  atom.atom("font-variant-ligatures", expression)
}

pub fn variant_numeric(value: NumericValue) -> atom.AtomOpt {
  let expression = case value {
    NormalNumeric -> expr.keyword("normal")
    ConfiguredNumeric(value) -> expr.function("", " ", numeric_values(value))
  }
  atom.atom("font-variant-numeric", expression)
}

fn ligature_values(value: Ligatures) -> List(expr.Expression) {
  case value {
    LigaturesCommon(common, discretionary, historical, contextual) -> [
      common_expression(common),
      ..optional_discretionary(discretionary)
      |> list.append(optional_historical(historical))
      |> list.append(optional_contextual(contextual))
    ]
    LigaturesDiscretionary(discretionary, historical, contextual) -> [
      discretionary_expression(discretionary),
      ..optional_historical(historical)
      |> list.append(optional_contextual(contextual))
    ]
    LigaturesHistorical(historical, contextual) -> [
      historical_expression(historical),
      ..optional_contextual(contextual)
    ]
    LigaturesContextual(contextual) -> [contextual_expression(contextual)]
  }
}

fn numeric_values(value: Numeric) -> List(expr.Expression) {
  case value {
    NumericLining(lining, proportional, fractions, ordinal, slashed_zero) -> [
      lining_expression(lining),
      ..optional_proportional(proportional)
      |> list.append(optional_fractions(fractions))
      |> list.append(optional_ordinal(ordinal))
      |> list.append(optional_slashed_zero(slashed_zero))
    ]
    NumericProportional(proportional, fractions, ordinal, slashed_zero) -> [
      proportional_expression(proportional),
      ..optional_fractions(fractions)
      |> list.append(optional_ordinal(ordinal))
      |> list.append(optional_slashed_zero(slashed_zero))
    ]
    NumericFractions(fractions, ordinal, slashed_zero) -> [
      fractions_expression(fractions),
      ..optional_ordinal(ordinal)
      |> list.append(optional_slashed_zero(slashed_zero))
    ]
    NumericOrdinal(ordinal, slashed_zero) -> [
      ordinal_expression(ordinal),
      ..optional_slashed_zero(slashed_zero)
    ]
    NumericSlashedZero(slashed_zero) -> [slashed_zero_expression(slashed_zero)]
  }
}

fn size_expression(value: SizeValue) -> expr.Expression {
  case value {
    SizeLength(value) -> expr.length(value)
    SizePercentage(value) -> expr.pct(value)
    SizeKeyword(value) -> expr.keyword(size_keyword_text(value))
    SizeLengthExpression(value) -> typed_expr.erase(value)
    SizePercentageExpression(value) -> typed_expr.erase(value)
    SizeKeywordExpression(value) -> typed_expr.erase(value)
  }
}

fn size_keyword_text(value: SizeKeyword) -> String {
  case value {
    Large -> "large"
    Math -> "math"
    Medium -> "medium"
    Small -> "small"
    XLarge -> "x-large"
    XSmall -> "x-small"
    XXLarge -> "xx-large"
    XXSmall -> "xx-small"
    XXXLarge -> "xxx-large"
  }
}

fn weight_expression(value: WeightValue) -> expr.Expression {
  case value {
    WeightNumber(value) -> expr.number(value)
    WeightKeyword(value) -> expr.keyword(weight_keyword_text(value))
    WeightNumberExpression(value) -> typed_expr.erase(value)
    WeightKeywordExpression(value) -> typed_expr.erase(value)
  }
}

fn weight_keyword_text(value: WeightKeyword) -> String {
  case value {
    Bold -> "bold"
    Bolder -> "bolder"
    Lighter -> "lighter"
    NormalWeight -> "normal"
  }
}

fn common_expression(value: CommonLigature) -> expr.Expression {
  case value {
    CommonLigatures -> expr.keyword("common-ligatures")
    NoCommonLigatures -> expr.keyword("no-common-ligatures")
    CommonLigaturesExpression(value) -> typed_expr.erase(value)
  }
}

fn optional_discretionary(
  value: Option(DiscretionaryLigature),
) -> List(expr.Expression) {
  case value {
    None -> []
    Some(value) -> [discretionary_expression(value)]
  }
}

fn discretionary_expression(value: DiscretionaryLigature) -> expr.Expression {
  case value {
    DiscretionaryLigatures -> expr.keyword("discretionary-ligatures")
    NoDiscretionaryLigatures -> expr.keyword("no-discretionary-ligatures")
    DiscretionaryLigaturesExpression(value) -> typed_expr.erase(value)
  }
}

fn optional_historical(
  value: Option(HistoricalLigature),
) -> List(expr.Expression) {
  case value {
    None -> []
    Some(value) -> [historical_expression(value)]
  }
}

fn historical_expression(value: HistoricalLigature) -> expr.Expression {
  case value {
    HistoricalLigatures -> expr.keyword("historical-ligatures")
    NoHistoricalLigatures -> expr.keyword("no-historical-ligatures")
    HistoricalLigaturesExpression(value) -> typed_expr.erase(value)
  }
}

fn optional_contextual(
  value: Option(ContextualLigature),
) -> List(expr.Expression) {
  case value {
    None -> []
    Some(value) -> [contextual_expression(value)]
  }
}

fn contextual_expression(value: ContextualLigature) -> expr.Expression {
  case value {
    ContextualLigatures -> expr.keyword("contextual-ligatures")
    NoContextualLigatures -> expr.keyword("no-contextual-ligatures")
    ContextualLigaturesExpression(value) -> typed_expr.erase(value)
  }
}

fn lining_expression(value: LiningNumbers) -> expr.Expression {
  case value {
    LiningNums -> expr.keyword("lining-nums")
    OldstyleNums -> expr.keyword("oldstyle-nums")
    LiningNumsExpression(value) -> typed_expr.erase(value)
    OldstyleNumsExpression(value) -> typed_expr.erase(value)
  }
}

fn optional_proportional(
  value: Option(ProportionalNumbers),
) -> List(expr.Expression) {
  case value {
    None -> []
    Some(value) -> [proportional_expression(value)]
  }
}

fn proportional_expression(value: ProportionalNumbers) -> expr.Expression {
  case value {
    ProportionalNums -> expr.keyword("proportional-nums")
    TabularNums -> expr.keyword("tabular-nums")
    ProportionalNumsExpression(value) -> typed_expr.erase(value)
    TabularNumsExpression(value) -> typed_expr.erase(value)
  }
}

fn optional_fractions(value: Option(FractionNumbers)) -> List(expr.Expression) {
  case value {
    None -> []
    Some(value) -> [fractions_expression(value)]
  }
}

fn fractions_expression(value: FractionNumbers) -> expr.Expression {
  case value {
    DiagonalFractions -> expr.keyword("diagonal-fractions")
    StackedFractions -> expr.keyword("stacked-fractions")
    DiagonalFractionsExpression(value) -> typed_expr.erase(value)
    StackedFractionsExpression(value) -> typed_expr.erase(value)
  }
}

fn optional_ordinal(value: Option(OrdinalNumber)) -> List(expr.Expression) {
  case value {
    None -> []
    Some(value) -> [ordinal_expression(value)]
  }
}

fn ordinal_expression(value: OrdinalNumber) -> expr.Expression {
  case value {
    Ordinal -> expr.keyword("ordinal")
    OrdinalExpression(value) -> typed_expr.erase(value)
  }
}

fn optional_slashed_zero(
  value: Option(SlashedZeroNumber),
) -> List(expr.Expression) {
  case value {
    None -> []
    Some(value) -> [slashed_zero_expression(value)]
  }
}

fn slashed_zero_expression(value: SlashedZeroNumber) -> expr.Expression {
  case value {
    SlashedZero -> expr.keyword("slashed-zero")
    SlashedZeroExpression(value) -> typed_expr.erase(value)
  }
}

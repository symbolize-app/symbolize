export namespace styleConfig {
  export let styleElement: HTMLStyleElement
  export let styleSheet: CSSStyleSheet
  export let styleRenderMap: Map<
    string,
    Map<Style, string[]>
  >
  export let styleNameCount: number = 0
  export let styleRenderCount: number

  export function init(document: Document) {
    styleConfig.styleElement = document.createElement(
      'style'
    )
    document.head.appendChild(styleConfig.styleElement)
    styleConfig.styleSheet = styleConfig.styleElement.sheet!
    styleConfig.styleRenderMap = new Map()
    styleConfig.styleRenderCount = 0
    console.log('STYLES', styleConfig)
  }
}

const styleBase = 36

type StyleProps = {
  readonly [TStyleName in keyof CSSStyleDeclaration]?: NonNullable<
    CSSStyleDeclaration[TStyleName]
  >
}

enum StyleNesting {
  Selector,
  CombinatorSelector,
}

type StyleSelector = readonly [
  {
    readonly nesting: StyleNesting.Selector
    readonly selector: string
  },
  StyleBody
]

type StyleCombinatorSelector = readonly [
  {
    readonly nesting: StyleNesting.CombinatorSelector
    readonly combinatorSelector: string
  },
  StyleBody
]

type StyleBody = readonly (
  | StyleProps
  | StyleSelector
  | StyleCombinatorSelector
  | Style
)[]

export type Style = {
  readonly name: string
  readonly body: StyleBody
}

export function style(body: StyleBody): Style {
  const styleNameClass = `s${styleConfig.styleNameCount.toString(
    styleBase
  )}`
  styleConfig.styleNameCount += 1
  return {
    name: styleNameClass,
    body: body,
  }
}

export function selector(
  selectorValue: string,
  body: StyleBody
): StyleSelector {
  return [
    {
      nesting: StyleNesting.Selector,
      selector: selectorValue,
    },
    body,
  ]
}

export function combinatorSelector(
  combinatorSelectorValue: string,
  body: StyleBody
): StyleCombinatorSelector {
  return [
    {
      nesting: StyleNesting.CombinatorSelector,
      combinatorSelector: combinatorSelectorValue,
    },
    body,
  ]
}

export function renderStyle(styleValue: Style): string[] {
  return renderNestedStyle(styleValue, '', '', false)
}

function renderNestedStyle(
  styleValue: Style,
  combinatorSelectorValue: string,
  selectorValue: string,
  newStyleName: boolean
): string[] {
  const fullSelectorValue = `${combinatorSelectorValue}${selectorValue}`
  let fullSelectorStyleMap = styleConfig.styleRenderMap.get(
    fullSelectorValue
  )
  if (!fullSelectorStyleMap) {
    fullSelectorStyleMap = new Map<Style, string[]>()
    styleConfig.styleRenderMap.set(
      fullSelectorValue,
      fullSelectorStyleMap
    )
  }

  const existingStyleClasses = fullSelectorStyleMap.get(
    styleValue
  )
  if (existingStyleClasses) {
    return existingStyleClasses
  }

  let styleName: string
  if (
    newStyleName &&
    (combinatorSelectorValue || selectorValue)
  ) {
    styleName = `r${styleConfig.styleRenderCount.toString(
      styleBase
    )}`
    styleConfig.styleRenderCount += 1
  } else {
    styleName = styleValue.name
  }
  const styleClasses = [styleName]
  for (const styleItem of styleValue.body) {
    if (Array.isArray(styleItem)) {
      const nestedStyleItem = styleItem as
        | StyleSelector
        | StyleCombinatorSelector
      const nesting = nestedStyleItem[0].nesting
      if (nesting === StyleNesting.Selector) {
        styleClasses.push(
          ...renderStyleSelector(
            styleName,
            nestedStyleItem as StyleSelector,
            combinatorSelectorValue,
            selectorValue
          )
        )
      } else {
        styleClasses.push(
          ...renderStyleCombinatorSelector(
            styleName,
            nestedStyleItem as StyleCombinatorSelector,
            combinatorSelectorValue,
            selectorValue
          )
        )
      }
    } else {
      const flatStyleItem = styleItem as StyleProps | Style
      if (flatStyleItem.hasOwnProperty('name')) {
        styleClasses.push(
          ...renderNestedStyle(
            flatStyleItem as Style,
            combinatorSelectorValue,
            selectorValue,
            true
          )
        )
      } else {
        renderStyleProps(
          styleName,
          flatStyleItem as StyleProps,
          combinatorSelectorValue,
          selectorValue
        )
      }
    }
  }
  fullSelectorStyleMap.set(styleValue, styleClasses)
  return Array.from(new Set(styleClasses))
}

function renderStyleSelector(
  styleName: string,
  styleValue: StyleSelector,
  combinatorSelectorValue: string,
  selectorValue: string
): string[] {
  return renderNestedStyle(
    { name: styleName, body: styleValue[1] },
    combinatorSelectorValue,
    selectorValue
      ? `${selectorValue}${styleValue[0].selector.substring(
          1
        )}`
      : styleValue[0].selector,
    false
  )
}

function renderStyleCombinatorSelector(
  styleName: string,
  styleValue: StyleCombinatorSelector,
  combinatorSelectorValue: string,
  selectorValue: string
): string[] {
  if (combinatorSelectorValue) {
    throw Error(
      `Style combinator "${combinatorSelectorValue}" already set, cannot use "${styleValue[0].combinatorSelector}"`
    )
  }
  return renderNestedStyle(
    { name: styleName, body: styleValue[1] },
    styleValue[0].combinatorSelector,
    selectorValue,
    false
  )
}

function renderStyleProps(
  styleName: string,
  styleValue: StyleProps,
  combinatorSelectorValue: string,
  selectorValue: string
): void {
  const styleRules = styleConfig.styleSheet.cssRules
  const styleIndex = styleRules.length
  styleConfig.styleSheet.insertRule(
    `${combinatorSelectorValue.substring(
      0,
      combinatorSelectorValue.length - 1
    )}.${styleName}${selectorValue.substring(1)} {}`,
    styleIndex
  )
  const styleRule = styleRules[styleIndex] as CSSStyleRule
  const styleDeclaration = styleRule.style
  Object.assign(styleDeclaration, styleValue)
}

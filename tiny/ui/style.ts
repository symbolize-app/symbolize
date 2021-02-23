export let configStyleElement: HTMLStyleElement
export let configStyleSheet: CSSStyleSheet
export let configStyleRenderMap: Map<
  string,
  Map<Style, string[]>
>
export let configStyleNameCount = 0
export let configStyleRenderCount: number

export function initConfig(document: Document): void {
  configStyleElement = document.createElement('style')
  document.head.appendChild(configStyleElement)
  const styleSheet = configStyleElement.sheet
  if (!styleSheet) {
    throw Error('Problem adding style element')
  }
  configStyleSheet = styleSheet
  configStyleRenderMap = new Map<
    string,
    Map<Style, string[]>
  >()
  configStyleRenderCount = 0
}

const styleBase = 36

type StyleProps = {
  readonly [TStyleName in keyof CSSStyleDeclaration]?: NonNullable<
    CSSStyleDeclaration[TStyleName]
  >
}

enum StyleNesting {
  selector,
  combinatorSelector,
}

type StyleSelector = readonly [
  {
    readonly nesting: StyleNesting.selector
    readonly selector: string
  },
  StyleBody
]

type StyleCombinatorSelector = readonly [
  {
    readonly nesting: StyleNesting.combinatorSelector
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

export function build(body: StyleBody): Style {
  const styleNameClass = `s${configStyleNameCount.toString(
    styleBase
  )}`
  configStyleNameCount += 1
  return {
    name: styleNameClass,
    body: body,
  }
}

export function useSelector(
  selectorValue: string,
  body: StyleBody
): StyleSelector {
  return [
    {
      nesting: StyleNesting.selector,
      selector: selectorValue,
    },
    body,
  ]
}

export function useCombinatorSelector(
  combinatorSelectorValue: string,
  body: StyleBody
): StyleCombinatorSelector {
  return [
    {
      nesting: StyleNesting.combinatorSelector,
      combinatorSelector: combinatorSelectorValue,
    },
    body,
  ]
}

export function render(style: Style): string[] {
  return renderNestedStyle(style, '', '', false)
}

function renderNestedStyle(
  style: Style,
  combinatorSelectorValue: string,
  selectorValue: string,
  newStyleName: boolean
): string[] {
  const fullSelectorValue = `${combinatorSelectorValue}${selectorValue}`
  let fullSelectorStyleMap = configStyleRenderMap.get(
    fullSelectorValue
  )
  if (!fullSelectorStyleMap) {
    fullSelectorStyleMap = new Map<Style, string[]>()
    configStyleRenderMap.set(
      fullSelectorValue,
      fullSelectorStyleMap
    )
  }

  const existingStyleClasses = fullSelectorStyleMap.get(
    style
  )
  if (existingStyleClasses) {
    return existingStyleClasses
  }

  let styleName: string
  if (
    newStyleName &&
    (combinatorSelectorValue || selectorValue)
  ) {
    styleName = `r${configStyleRenderCount.toString(
      styleBase
    )}`
    configStyleRenderCount += 1
  } else {
    styleName = style.name
  }
  const styleClasses = [styleName]
  for (const styleItem of style.body) {
    if (Array.isArray(styleItem)) {
      const nestedStyleItem = styleItem as
        | StyleSelector
        | StyleCombinatorSelector
      const nesting = nestedStyleItem[0].nesting
      if (nesting === StyleNesting.selector) {
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
      if (Reflect.has(flatStyleItem, 'name')) {
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
  fullSelectorStyleMap.set(style, styleClasses)
  return Array.from(new Set(styleClasses))
}

function renderStyleSelector(
  styleName: string,
  style: StyleSelector,
  combinatorSelectorValue: string,
  selectorValue: string
): string[] {
  return renderNestedStyle(
    { name: styleName, body: style[1] },
    combinatorSelectorValue,
    selectorValue
      ? `${selectorValue}${style[0].selector.substring(1)}`
      : style[0].selector,
    false
  )
}

function renderStyleCombinatorSelector(
  styleName: string,
  style: StyleCombinatorSelector,
  combinatorSelectorValue: string,
  selectorValue: string
): string[] {
  if (combinatorSelectorValue) {
    throw Error(
      `Style combinator "${combinatorSelectorValue}" already set, cannot use "${style[0].combinatorSelector}"`
    )
  }
  return renderNestedStyle(
    { name: styleName, body: style[1] },
    style[0].combinatorSelector,
    selectorValue,
    false
  )
}

function renderStyleProps(
  styleName: string,
  style: StyleProps,
  combinatorSelectorValue: string,
  selectorValue: string
): void {
  const styleRules = configStyleSheet.cssRules
  const styleIndex = styleRules.length
  configStyleSheet.insertRule(
    `${combinatorSelectorValue.substring(
      0,
      combinatorSelectorValue.length - 1
    )}.${styleName}${selectorValue.substring(1)} {}`,
    styleIndex
  )
  const styleRule = styleRules[styleIndex] as CSSStyleRule
  const styleDeclaration = styleRule.style
  Object.assign(styleDeclaration, style)
}

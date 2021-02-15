export namespace styleConfig {
  export let styleSheet: CSSStyleSheet
  export let styleMap: Map<string, Map<Style, string[]>>
  export let styleCount: number
}

export function initStyles() {
  const styleElement = document.createElement('style')
  document.head.appendChild(styleElement)
  styleConfig.styleSheet = styleElement.sheet!
  styleConfig.styleMap = new Map()
  styleConfig.styleCount = 0
  console.log('STYLES', styleConfig)
}

export type Style =
  | StyleProps
  | StylePseudo
  | StyleMerge
  | false
  | undefined
  | ''
  | 0

type StyleProps = {
  [TStyleName in keyof CSSStyleDeclaration]?: NonNullable<
    CSSStyleDeclaration[TStyleName]
  >
}

type StylePseudo = [string, Style]

type StyleMerge = Style[]

export function renderStyle(styleValue: Style): string[] {
  return renderStyleForPseudo(styleValue, '')
}

function renderStyleForPseudo(
  styleValue: Style,
  pseudo: string
): string[] {
  let pseudoStyleMap = styleConfig.styleMap.get(pseudo)
  if (!pseudoStyleMap) {
    pseudoStyleMap = new Map<Style, string[]>()
    styleConfig.styleMap.set(pseudo, pseudoStyleMap)
  }
  let styleClasses = pseudoStyleMap.get(styleValue)
  if (!styleClasses) {
    if (!styleValue) {
      styleClasses = []
    } else if (isStylePseudoOrMerge(styleValue)) {
      if (isStylePseudo(styleValue)) {
        styleClasses = renderStylePseudo(styleValue, pseudo)
      } else {
        styleClasses = renderStyleMerge(styleValue, pseudo)
      }
    } else {
      styleClasses = renderStyleProps(styleValue, pseudo)
    }
    pseudoStyleMap.set(styleValue, styleClasses)
  }
  return styleClasses
}

function renderStylePseudo(
  styleValue: StylePseudo,
  pseudo: string
): string[] {
  const [nextPseudo, nextStyleValue] = styleValue
  return renderStyleForPseudo(
    nextStyleValue,
    `${pseudo}${nextPseudo}`
  )
}

function renderStyleMerge(
  styleValue: StyleMerge,
  pseudo: string
): string[] {
  return ([] as string[]).concat(
    ...styleValue.map((newStyle) =>
      renderStyleForPseudo(newStyle, pseudo)
    )
  )
}

function renderStyleProps(
  styleValue: StyleProps,
  pseudo: string
): string[] {
  const styleBase = 36
  const styleClass = `s${styleConfig.styleCount.toString(
    styleBase
  )}`
  styleConfig.styleCount += 1
  const styleRules = styleConfig.styleSheet.cssRules
  const styleIndex = styleRules.length
  styleConfig.styleSheet.insertRule(
    `.${styleClass}${pseudo} {}`,
    styleIndex
  )
  const styleRule = styleRules[styleIndex] as CSSStyleRule
  const styleDeclaration = styleRule.style
  Object.assign(styleDeclaration, styleValue)
  return [styleClass]
}

function isStylePseudoOrMerge(
  styleValue: StylePseudo | StyleMerge | StyleProps
): styleValue is StylePseudo | StyleMerge {
  return styleValue.constructor === Array
}

function isStylePseudo(
  styleValue: StylePseudo | StyleMerge
): styleValue is StylePseudo {
  return typeof styleValue[0] === 'string'
}

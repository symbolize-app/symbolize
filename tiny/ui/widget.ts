import { MutableKeys } from 'utility-types'
import {
  renderStyle,
  Style,
  styleConfig,
} from '@tiny/ui/style'

export namespace widgetConfig {
  export let document: Document

  export function init(document: Document) {
    widgetConfig.document = document
  }
}

const listeners = Symbol('listeners')

const listenerOptions = Symbol('listenerOptions')

export function advancedListener<T>(
  body: T,
  options: AddEventListenerOptions
): T & { [listenerOptions]: AddEventListenerOptions } {
  return Object.assign(body, {
    [listenerOptions]: options,
  })
}

export type HtmlListeners<E extends Element = Element> = {
  [K in keyof HTMLElementEventMap]?: ((
    this: E,
    ev: HTMLElementEventMap[K]
  ) => any) & {
    [listenerOptions]?: AddEventListenerOptions
  }
}

type BodyWidget = {
  body: Widget
}

type RangeWidget = {
  content: Widget[]
}

export type Widget =
  | Node
  | BodyWidget
  | RangeWidget
  | string
  | false
  | undefined
  | null

function replaceChildren(
  parent: ParentNode,
  children: (Node | string)[]
): void {
  if (parent instanceof HTMLHeadElement) {
    let styleElement: HTMLStyleElement | null = null
    let node = parent.firstChild
    while (node) {
      const nextNode = node.nextSibling
      if (node !== styleConfig.styleElement) {
        node.remove()
      } else if (!styleElement) {
        styleElement = styleConfig.styleElement
      }
      node = nextNode
    }
    for (const child of children) {
      parent.insertBefore(
        child instanceof Node
          ? child
          : widgetConfig.document.createTextNode(child),
        styleElement
      )
    }
  } else {
    // TODO Check for replaceChilren
    parent.replaceChildren(...children)
  }
}

const elementProperties = {
  styles: {
    set(this: Element, value: Style[]) {
      if (this.classList.length) {
        this.classList.remove(...this.classList)
      }
      for (const styleValue of value) {
        this.classList.add(...renderStyle(styleValue))
      }
    },
  },
  listen: {
    set(
      this: Element & {
        [listeners]: Record<
          string,
          EventListener & {
            [listenerOptions]?: AddEventListenerOptions
          }
        >
      },
      value: Record<
        string,
        EventListener & {
          [listenerOptions]?: AddEventListenerOptions
        }
      >
    ) {
      const oldListeners = this[listeners]
      for (const key in oldListeners) {
        const listener = oldListeners[key]
        this.removeEventListener(
          key,
          listener,
          listener[listenerOptions]
        )
      }
      for (const key in value) {
        const listener = value[key]
        this.addEventListener(
          key,
          listener,
          listener[listenerOptions]
        )
      }
    },
  },
  [listeners]: {
    writable: true,
    value: {},
  },
  content: {
    set(this: Element, value: Widget[]) {
      replaceChildren(this, collect(value))
    },
  },
}

function collect(items: Widget[]): (string | Node)[] {
  const results: (string | Node)[] = []

  function loop(item: Widget) {
    if (typeof item == 'string' || item instanceof Node) {
      results.push(item)
    } else if (
      item === false ||
      item === undefined ||
      item === null
    ) {
    } else if (item.hasOwnProperty('body')) {
      loop((item as BodyWidget).body)
    } else {
      for (const subitem of (item as RangeWidget).content) {
        loop(subitem)
      }
    }
  }

  for (let item of items) {
    loop(item)
  }

  return results
}

export type WidgetInitializer<
  T extends Widget & object
> = Partial<Pick<T, MutableKeys<T>>>

export type WidgetFunction<T extends Widget & object> = (
  data: WidgetInitializer<T>
) => T

export function widget<T extends Widget & object>(
  body: () => T
): WidgetFunction<T> {
  return (data) => {
    return Object.assign(body(), data)
  }
}

type HtmlWidget<T extends HTMLElement> = T & {
  styles: Style[]
  listen: HtmlListeners<T>
  content: Widget[]
}

export function toHtmlWidget<T extends HTMLElement>(
  element: T
): HtmlWidget<T> {
  return Object.defineProperties(element, elementProperties)
}

type HtmlWidgetMap = {
  [K in keyof HTMLElementTagNameMap]: WidgetFunction<
    HtmlWidget<HTMLElementTagNameMap[K]>
  >
}

export const html: HtmlWidgetMap = new Proxy(
  {} as HtmlWidgetMap,
  {
    get(
      target: HtmlWidgetMap,
      property: keyof HTMLElementTagNameMap,
      _receiver: unknown
    ) {
      return (
        target[property] ||
        (target[property] = widget(() =>
          toHtmlWidget(
            widgetConfig.document.createElement(property)
          )
        ) as any)
      )
    },
  }
)

export const range = widget<{
  content: Widget[]
}>(() => {
  const start = widgetConfig.document.createComment('')
  const end = widgetConfig.document.createComment('')
  const content: Widget[] = [start, end]

  return {
    get content() {
      return content
    },
    set content(value: Widget[]) {
      const inner = collect(value)
      const parent = start.parentNode
      if (parent) {
        const siblings: (string | Node)[] = Array.from(
          parent.childNodes
        )
        siblings.splice(
          siblings.indexOf(start) + 1,
          content.length - 2,
          ...inner
        )
        replaceChildren(parent, siblings)
      }
      content.splice(1, content.length - 2, ...inner)
    },
  }
})

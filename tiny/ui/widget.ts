import * as style from '@tiny/ui/style.ts'
import * as utilityTypes from 'utility-types'

export let configDocument: Document

export function initConfig(document: Document): void {
  configDocument = document
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
  ) => void) & {
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
      if (node !== style.configStyleElement) {
        node.remove()
      } else if (!styleElement) {
        styleElement = style.configStyleElement
      }
      node = nextNode
    }
    for (const child of children) {
      parent.insertBefore(
        child instanceof Node
          ? child
          : configDocument.createTextNode(child),
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
    set(this: Element, value: style.Style[]) {
      if (this.classList.length) {
        this.classList.remove(...this.classList)
      }
      for (const styleItem of value) {
        this.classList.add(...style.render(styleItem))
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
      // Skip
    } else if (Reflect.has(item, 'body')) {
      loop((item as BodyWidget).body)
    } else {
      for (const subitem of (item as RangeWidget).content) {
        loop(subitem)
      }
    }
  }

  for (const item of items) {
    loop(item)
  }

  return results
}

export type WidgetInitializer<
  T extends Widget & { [K in keyof T]: T[K] }
> = Partial<Pick<T, utilityTypes.MutableKeys<T>>>

export type WidgetFunction<
  T extends Widget & { [K in keyof T]: T[K] }
> = (data: WidgetInitializer<T>) => T

export function define<
  T extends Widget & { [K in keyof T]: T[K] }
>(body: () => T): WidgetFunction<T> {
  return (data) => {
    return Object.assign(body(), data)
  }
}

type HtmlWidget<T extends HTMLElement> = T & {
  styles: style.Style[]
  listen: HtmlListeners<T>
  content: Widget[]
}

export function toHtmlWidget<T extends HTMLElement>(
  element: T
): HtmlWidget<T> {
  return Object.defineProperties(
    element,
    elementProperties
  ) as HtmlWidget<T>
}

type HtmlWidgetMap = {
  [K in keyof HTMLElementTagNameMap]: WidgetFunction<
    HtmlWidget<HTMLElementTagNameMap[K]>
  >
}

export const html: HtmlWidgetMap = new Proxy(
  {} as HtmlWidgetMap,
  {
    get<K extends keyof HTMLElementTagNameMap>(
      target: unknown,
      property: K
    ) {
      return ((target as Record<
        K,
        WidgetFunction<HtmlWidget<HTMLElementTagNameMap[K]>>
      >)[property] ??= define(() =>
        toHtmlWidget(
          configDocument.createElement(property)
        )))
    },
  }
)

export const range = define<{
  content: Widget[]
}>(() => {
  const start = configDocument.createComment('')
  const end = configDocument.createComment('')
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

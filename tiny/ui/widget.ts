import { MutableKeys } from 'utility-types'

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

export type Style = any

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
  // TODO Check for replaceChilren
  parent.replaceChildren(...children)
}

const elementProperties = {
  styles: {
    set(_value: never) {},
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

type HtmlWidgetMap = {
  [K in keyof HTMLElementTagNameMap]: WidgetFunction<
    HTMLElementTagNameMap[K] & {
      styles: Style[]
      listen: HtmlListeners<HTMLElementTagNameMap[K]>
      content: Widget[]
    }
  >
}

export const html: HtmlWidgetMap = new Proxy(
  {} as HtmlWidgetMap,
  {
    get(
      target: HtmlWidgetMap,
      property: keyof HtmlWidgetMap,
      _receiver
    ) {
      return (
        target[property] ||
        (target[property] = widget(() =>
          Object.defineProperties(
            document.createElement(property),
            elementProperties
          )
        ))
      )
    },
  }
)

export const range = widget<{
  content: Widget[]
}>(() => {
  const start = document.createComment('')
  const end = document.createComment('')
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

export function replaceHtml(widget: Widget) {
  const result = collect([widget])
  if (result.length !== 1) {
    throw Error('Too many elements to replace HTML')
  } else if (!(result[0] instanceof HTMLHtmlElement)) {
    throw Error('Only HTML element can replace HTML')
  }
  document.replaceChild(result[0], document.documentElement)
}

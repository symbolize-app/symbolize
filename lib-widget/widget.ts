import * as tinyStyle from '@intertwine/lib-style/style.ts'

const listeners = Symbol('listeners')

const context = Symbol('context')

const listenerOptions = Symbol('listenerOptions')

export function advancedListener<
  T extends Record<string, unknown>,
>(
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

export type Context = {
  window: NonNullable<Document['defaultView']>
  document: Document & {
    head: HtmlWidget<HTMLHeadElement>
    body: HtmlWidget<HTMLBodyElement>
    defaultView: NonNullable<Document['defaultView']>
  }
} & tinyStyle.Context

export function initContext(document: Document): Context {
  if (!document.defaultView) {
    throw new Error('No document default view')
  }
  const ctx = {
    ...tinyStyle.initContext(document),
    document,
    window: document.defaultView,
  } as Context
  toHtmlWidget(ctx, document.body)
  toHtmlWidget(ctx, document.head)
  return ctx
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
  ctx: Context,
  parent: Node & ParentNode,
  children: (Node | string)[]
): void {
  if (parent instanceof ctx.window.HTMLHeadElement) {
    let styleElement: HTMLStyleElement | null = null
    let node = parent.firstChild
    while (node) {
      const nextNode = node.nextSibling
      if (node !== ctx.styleElement) {
        node.remove()
      } else if (!styleElement) {
        styleElement = ctx.styleElement
      }
      node = nextNode
    }
    for (const child of children) {
      parent.insertBefore(
        typeof child === 'string'
          ? ctx.document.createTextNode(child)
          : child,
        styleElement
      )
    }
  } else if (parent.replaceChildren) {
    parent.replaceChildren(...children)
  } else {
    let node = parent.firstChild
    while (node) {
      const nextNode = node.nextSibling
      node.remove()
      node = nextNode
    }
    for (const child of children) {
      parent.appendChild(
        typeof child === 'string'
          ? ctx.document.createTextNode(child)
          : child
      )
    }
  }
}

const elementProperties = {
  [context]: {
    writable: true,
    value: {},
  },
  styles: {
    set(
      this: Element & { [context]: Context },
      value: tinyStyle.Style[]
    ) {
      if (this.classList.length) {
        this.classList.remove(...this.classList)
      }
      for (const styleItem of value) {
        this.classList.add(
          ...tinyStyle.render(this[context], styleItem)
        )
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
    set(
      this: Element & { [context]: Context },
      value: Widget[]
    ) {
      replaceChildren(this[context], this, collect(value))
    },
  },
}

export function collect(
  items: Widget[]
): (string | Node)[] {
  const results: (string | Node)[] = []

  function loop(item: Widget) {
    if (
      item === false ||
      item === undefined ||
      item === null
    ) {
      // Skip
    } else if (
      typeof item == 'string' ||
      Reflect.has(item, 'nodeType')
    ) {
      results.push(item as string | Node)
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

type WidgetFunction<
  Body extends Widget & { [Key in keyof Body]: Body[Key] },
  CustomContext = unknown,
> = (
  ctx: CustomContext & Context,
  data: Partial<Body>
) => Body

export function define<
  Body extends Widget & { [Key in keyof Body]: Body[Key] },
  CustomContext,
>(
  body: (ctx: CustomContext & Context) => Body
): WidgetFunction<Body, CustomContext> {
  return (ctx, data) => {
    return Object.assign(body(ctx), data)
  }
}

export type HtmlWidget<T extends HTMLElement> = T & {
  styles: tinyStyle.Style[]
  listen: HtmlListeners<T>
  content: Widget[]
}

export function toHtmlWidget<T extends HTMLElement>(
  ctx: Context,
  element: T
): HtmlWidget<T> {
  const widget = Object.defineProperties(
    element,
    elementProperties
  ) as HtmlWidget<T>
  ;(widget as unknown as { [context]: Context })[context] =
    ctx
  return widget
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
      return ((
        target as Record<
          K,
          WidgetFunction<
            HtmlWidget<HTMLElementTagNameMap[K]>
          >
        >
      )[property] ??= define((ctx) =>
        toHtmlWidget(
          ctx,
          ctx.document.createElement(property)
        )
      ))
    },
  }
)

export const range = define(
  (
    ctx: Context
  ): {
    content: Widget[]
  } => {
    const start = ctx.document.createComment('')
    const end = ctx.document.createComment('')
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
          replaceChildren(ctx, parent, siblings)
        }
        content.splice(1, content.length - 2, ...inner)
      },
    }
  }
)

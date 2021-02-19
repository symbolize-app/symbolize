import { MutableKeys } from 'utility-types'
import { Overwrite } from 'utility-types'

const listeners = Symbol('listeners')

const listenerOptions = Symbol('listenerOptions')

function advancedListener<T>(
  body: T,
  options: AddEventListenerOptions
) {
  return Object.assign(body, {
    [listenerOptions]: options,
  })
}

type Listeners = any

type Widget =
  | string
  | Node
  | false
  | undefined
  | null
  | { root: (string | Node)[] }

type ElementWidget<T> = any

function replaceChildren(
  parent: ParentNode,
  children: (Node | string)[]
): void {
  // TODO Check for replaceChilren
  parent.replaceChildren(...children)
}

const elementProperties = {
  classes: {
    set(_value: never) {},
  },
  inner: {
    set(this: Element, value: Widget[]) {
      replaceChildren(this, collect(value))
    },
  },
  listen: {
    set(
      this: Element & { [listeners]: Listeners },
      value: Listeners
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
}

function collect(items: Widget[]): (string | Node)[] {
  const results = []
  for (let item of items) {
    if (typeof item == 'string' || item instanceof Node) {
      results.push(item)
    } else if (
      item === false ||
      item === undefined ||
      item === null
    ) {
    } else {
      for (const subitem of item.root) {
        results.push(subitem)
      }
    }
  }
  return results
}

type WidgetInitializer<T extends object> = Simplify<
  Partial<Pick<T, MutableKeys<T>>>
>

type WidgetBody<T extends object> = T extends Node
  ? T
  : Overwrite<T, { readonly root: Widget }>

function widget<T extends object>(
  body: () => WidgetBody<T>
): (data: WidgetInitializer<T>) => WidgetBody<T> {
  return (data) => {
    return Object.assign(body(), data)
  }
}

const html = new Proxy(
  {},
  {
    get(target, property, _receiver) {
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

const range = widget(() => {
  const start = document.createComment('')
  const end = document.createComment('')
  const root = [start, end]

  return {
    root,
    set inner(value: Widget[]) {
      const inner = collect(value)
      const parent = start.parentNode
      if (parent) {
        const siblings = Array.from(parent.childNodes)
        siblings.splice(
          siblings.indexOf(start) + 1,
          root.length - 2,
          ...inner
        )
        replaceChildren(parent, siblings)
      }
      root.splice(1, root.length - 2, ...inner)
    },
  }
})

function replaceHtml(
  widget: ElementWidget<HTMLHtmlElement>
) {
  document.replaceChild(
    collect([widget])[0],
    document.documentElement
  )
}

const myButton = widget<{
  readonly root: Widget
  listen: number
}>(() => {
  const root = html.button({
    inner: ['OK'],
  })
  const body: {
    readonly root: Widget
    listen: number
  } = {
    root,
    set listen(value: number) {
      root.listen = value
    },
  }
  return body
})

const myCounter = widget(() => {
  let value
  const root = html.span({})
  const body = {
    root: [root],
    get value() {
      return value
    },
    set value(_value) {
      value = _value
      root.inner = [value.toString()]
    },
  }
  body.value = 0
  return body
})

const counter = myCounter()

const listContents = range()

const div = html.div({
  inner: [
    'HI ',
    myButton({
      listen: {
        click() {
          counter.value += 1
          listContents.inner = [
            html.li({
              inner: [counter.value.toString()],
            }),
            html.li({
              inner: [(counter.value * 2).toString()],
            }),
          ]
        },
      },
    }),
    ' ',
    counter,
    html.ul({
      inner: [
        html.li({
          inner: ['+'],
        }),
        listContents,
        html.li({
          inner: ['-'],
        }),
      ],
    }),
  ],
})

replaceHtml(
  html.html({
    inner: [
      html.body({
        inner: [div],
      }),
    ],
  })
)

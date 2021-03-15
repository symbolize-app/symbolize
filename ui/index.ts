import * as message from '@fe/core/message.ts'
import * as button from '@fe/ui/button.ts'
import * as style from '@tiny/ui/style.ts'
import * as widget from '@tiny/ui/widget.ts'

console.log(message.hi)

const green = style.build([
  {
    backgroundColor: 'green',
  },
])
const blue = style.build([
  {
    backgroundColor: 'blue',
  },
])
const red = style.build([
  {
    backgroundColor: 'red',
  },
  blue,
  style.useSelector('&:hover', [green]),
])
const bold = style.build([
  {
    fontWeight: 'bold',
  },
])

const div = widget.html.div
const li = widget.html.li
const link = widget.html.link
const span = widget.html.span
const title = widget.html.title
const ul = widget.html.ul

const myCounter = widget.define<{
  readonly body: widget.Widget
  value: number
}>((ctx) => {
  let value: number
  const body = span(ctx, {})
  const result = {
    body,
    get value() {
      return value
    },
    set value(newValue) {
      value = newValue
      body.content = [value.toString()]
    },
  }
  result.value = 0
  return result
})

async function main(): Promise<void> {
  const ctx = widget.initContext(window.document)

  const counter = myCounter(ctx, {})

  const listContents = widget.range(ctx, {
    content: [
      li(ctx, {
        styles: [bold],
        content: ['init'],
      }),
    ],
  })

  function onClick(event: MouseEvent): void {
    console.log(event)
    counter.value += 1
    listContents.content = [
      li(ctx, {
        styles: [red],
        content: [counter.value.toString()],
      }),
      li(ctx, {
        content: [(counter.value * 2).toString()],
      }),
    ]
  }

  const rootDiv = div(ctx, {
    content: [
      'HI ',
      button.custom(ctx, {
        listen: {
          click: onClick,
        },
      }),
      ' ',
      counter,
      ul(ctx, {
        content: [
          li(ctx, {
            content: ['+'],
          }),
          listContents,
          li(ctx, {
            content: ['-'],
          }),
        ],
      }),
    ],
  })

  const head = widget.toHtmlWidget(
    ctx,
    window.document.head
  )
  const body = widget.toHtmlWidget(
    ctx,
    window.document.body
  )

  head.content = [
    title(ctx, { content: ['Fertile Earth'] }),
    link(ctx, {
      rel: 'icon',
      href: 'data:;base64,iVBORw0KGgo=',
    }),
  ]
  body.content = [rootDiv]

  console.log(
    'SERVER',
    await (await fetch('/api/message')).text()
  )

  if (import.meta.env.NODE_ENV === 'development') {
    const dev = await import('@fe/ui/dev.ts')
    dev.main()
  }
}

main().catch(console.error)

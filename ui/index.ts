import * as message from '@fe/core/message.ts'
import * as button from '@fe/ui/button.ts'
import type * as uiContext from '@fe/ui/context.ts'
import * as uiMember from '@fe/ui/member.ts'
import * as submit from '@tiny/api/submit.ts'
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
  body: widget.Widget
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
  const ctx: uiContext.Context = {
    ...widget.initContext(window.document),
    ...submit.initContext(window),
    performanceNow: () => window.performance.now(),
    setTimeout: (...args) => window.setTimeout(...args),
    random: () => Math.random(),
  }

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
      uiMember.custom(ctx, {}),
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

  ctx
    .submit({ url: '/api/message', method: 'GET' })
    .then(async (response) => {
      console.log('SERVER', await response.text())
    })
    .catch(console.error)

  if (import.meta.env.NODE_ENV === 'development') {
    const dev = await import('@fe/ui/dev.ts')
    dev.main()
  }
}

main().catch(console.error)

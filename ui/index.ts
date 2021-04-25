import * as appMessage from '@fe/core/message.ts'
import * as appWidgetButton from '@fe/ui/widget/button.ts'
import * as appWidgetMember from '@fe/ui/widget/member.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as random from '@tiny/core/random.ts'
import * as timeBrowser from '@tiny/core/time.browser.ts'
import * as style from '@tiny/ui/style.ts'
import * as submit from '@tiny/ui/submit.ts'
import * as widget from '@tiny/ui/widget.ts'

console.log(appMessage.hi)

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
  const ctx: widget.Context &
    errorModule.Context &
    submit.Context = {
    ...random.initContext(),
    ...timeBrowser.initContext(),
    ...widget.initContext(window.document),
    ...submit.initContext(window),
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
      appWidgetButton.custom(ctx, {
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
      appWidgetMember.custom(ctx, {}),
    ],
  })

  const head = ctx.document.head
  const body = ctx.document.body

  head.content = [
    title(ctx, { content: ['Fertile Earth'] }),
    link(ctx, {
      rel: 'icon',
      href: 'data:;base64,iVBORw0KGgo=',
    }),
  ]
  body.content = [rootDiv]

  ctx
    .submit({ path: '/api/message', method: 'GET' })
    .then(async (response) => {
      console.log('SERVER', await response.text())
    })
    .catch(console.error)

  if (import.meta.env.NODE_ENV === 'development') {
    const dev = await import('@fe/ui/dev.ts')
    dev.main(ctx)
  }
}

main().catch(console.error)

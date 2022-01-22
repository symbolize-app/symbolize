import * as appMessage from '@fe/core/message.ts'
import * as appSubmit from '@fe/core/submit.ts'
import * as appWidgetButton from '@fe/ui/widget/button.ts'
import * as appWidgetFile from '@fe/ui/widget/file.ts'
import * as appWidgetMember from '@fe/ui/widget/member.ts'
import * as appWidgetSearch from '@fe/ui/widget/search.ts'
import * as appWidgetTopic from '@fe/ui/widget/topic.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinyRandom from '@tiny/core/random.ts'
import * as tinySubmit from '@tiny/core/submit.ts'
import * as tinyTimeBrowser from '@tiny/core/time.browser.ts'
import * as tinyStyle from '@tiny/ui/style.ts'
import * as tinyWidget from '@tiny/ui/widget.ts'

console.log(appMessage.hi)

const green = tinyStyle.build([
  {
    backgroundColor: 'green',
  },
])
const blue = tinyStyle.build([
  {
    backgroundColor: 'blue',
  },
])
const red = tinyStyle.build([
  {
    backgroundColor: 'red',
  },
  blue,
  tinyStyle.useSelector('&:hover', [green]),
])
const bold = tinyStyle.build([
  {
    fontWeight: 'bold',
  },
])

const div = tinyWidget.html.div
const li = tinyWidget.html.li
const link = tinyWidget.html.link
const span = tinyWidget.html.span
const title = tinyWidget.html.title
const ul = tinyWidget.html.ul

const myCounter = tinyWidget.define(
  (
    ctx: tinyWidget.Context
  ): {
    body: tinyWidget.Widget
    value: number
  } => {
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
  }
)

async function main(): Promise<void> {
  const ctx: tinyWidget.Context &
    tinyError.Context &
    tinySubmit.Context = {
    ...tinyRandom.initContext(),
    ...tinyTimeBrowser.initContext(),
    ...tinyWidget.initContext(window.document),
    ...tinySubmit.initContext(
      window,
      appSubmit.retryConfig
    ),
  }

  const counter = myCounter(ctx, {})

  const listContents = tinyWidget.range(ctx, {
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
      appWidgetMember.create(ctx, {}),
      appWidgetTopic.list(ctx, {}),
      appWidgetSearch.query(ctx, {}),
      appWidgetFile.query(ctx, {}),
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

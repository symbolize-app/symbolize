import type * as tinyError from '@intertwine/lib-error/error.ts'
import * as tinyRandom from '@intertwine/lib-random/random.ts'
import * as tinyStyle from '@intertwine/lib-style/style.ts'
import * as tinyTimeBrowser from '@intertwine/lib-time/time.browser.ts'
import * as tinyWidget from '@intertwine/lib-widget/widget.ts'

import * as appWidgetButton from '@/widget/button.ts'
import * as appWidgetFile from '@/widget/file.ts'
import * as appWidgetMember from '@/widget/member.ts'
import * as appWidgetSearch from '@/widget/search.ts'
import * as appWidgetTopic from '@/widget/topic.ts'

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
  const ctx: tinyWidget.Context & tinyError.Context = {
    ...tinyRandom.initContext(),
    ...tinyTimeBrowser.initContext(),
    ...tinyWidget.initContext(window.document),
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

  if (import.meta.env.NODE_ENV === 'development') {
    const dev = await import('@/dev.ts')
    dev.main(ctx)
  }
}

main().catch(console.error)

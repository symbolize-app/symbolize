import * as svcWidgetButton from '@/widget/button.ts'
import * as svcWidgetFile from '@/widget/file.ts'
import * as svcWidgetMember from '@/widget/member.ts'
import * as svcWidgetSearch from '@/widget/search.ts'
import * as svcWidgetTopic from '@/widget/topic.ts'
import type * as error from '@intertwine/lib-error'
import * as random from '@intertwine/lib-random'
import * as style from '@intertwine/lib-style'
import * as timeBrowser from '@intertwine/lib-time/index.browser.ts'
import * as widget from '@intertwine/lib-widget'

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

const myCounter = widget.define(
  (
    ctx: widget.Context
  ): {
    body: widget.Widget
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

function main(): void {
  const ctx: widget.Context & error.Context = {
    ...random.initContext(),
    ...timeBrowser.initContext(),
    ...widget.initContext(window.document),
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
      svcWidgetButton.custom(ctx, {
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
      svcWidgetMember.create(ctx, {}),
      svcWidgetTopic.list(ctx, {}),
      svcWidgetSearch.query(ctx, {}),
      svcWidgetFile.query(ctx, {}),
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
}

main()

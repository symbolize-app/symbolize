import { initStyles } from '@tiny/ui/style'
import { style } from '@tiny/ui/style'
import { renderStyle } from '@tiny/ui/style'
import { Widget } from '@tiny/ui/widget'
import { html } from '@tiny/ui/widget'
import { widget } from '@tiny/ui/widget'
import { HtmlListeners } from '@tiny/ui/widget'
import { range } from '@tiny/ui/widget'
import { replaceHtml } from '@tiny/ui/widget'
import { message } from '@fe/ui/message'

console.log(message)
initStyles()
const s = style([
  {
    color: 'red',
  },
])
const r = renderStyle(s)
console.log('CLASSES', r)

const myButton = widget<{
  readonly body: Widget
  listen: HtmlListeners
}>(() => {
  const body = html.button({
    content: ['OK'],
  })
  return {
    body,
    set listen(value: HtmlListeners) {
      body.listen = value
    },
  }
})

const myCounter = widget<{
  readonly body: Widget
  value: number
}>(() => {
  let value: number
  const body = html.span({})
  const result = {
    body,
    get value() {
      return value
    },
    set value(_value) {
      value = _value
      body.content = [value.toString()]
    },
  }
  result.value = 0
  return result
})

const counter = myCounter({})

const listContents = range({
  content: [
    html.li({
      content: ['init'],
    }),
  ],
})

const div = html.div({
  content: [
    'HI ',
    myButton({
      listen: {
        click(event) {
          console.log(event)
          counter.value += 1
          listContents.content = [
            html.li({
              content: [counter.value.toString()],
            }),
            html.li({
              content: [(counter.value * 2).toString()],
            }),
          ]
        },
      },
    }),
    ' ',
    counter,
    html.ul({
      content: [
        html.li({
          content: ['+'],
        }),
        listContents,
        html.li({
          content: ['-'],
        }),
      ],
    }),
  ],
})

replaceHtml(
  html.html({
    content: [
      html.body({
        content: [div],
      }),
    ],
  })
)

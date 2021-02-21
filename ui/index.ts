import { styleConfig } from '@tiny/ui/style'
import { style } from '@tiny/ui/style'
import { selector } from '@tiny/ui/style'
import { Widget } from '@tiny/ui/widget'
import { widgetConfig } from '@tiny/ui/widget'
import { html } from '@tiny/ui/widget'
import { widget } from '@tiny/ui/widget'
import { HtmlListeners } from '@tiny/ui/widget'
import { range } from '@tiny/ui/widget'
import { toHtmlWidget } from '@tiny/ui/widget'
import { message } from '@fe/ui/message'

widgetConfig.init(window.document)
styleConfig.init(window.document)

console.log(message)

const green = style([
  {
    backgroundColor: 'green',
  },
])
const blue = style([
  {
    backgroundColor: 'blue',
  },
])
const red = style([
  {
    backgroundColor: 'red',
  },
  blue,
  selector('&:hover', [green]),
])
const bold = style([
  {
    fontWeight: 'bold',
  },
])

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
      styles: [bold],
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
              styles: [red],
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

const head = toHtmlWidget(window.document.head)
const body = toHtmlWidget(window.document.body)

head.content = [html.title({ content: ['Fertile Earth'] })]
body.content = [div]

import type * as tinyError from '@intertwine/error/error.ts'
import type * as tinyRandom from '@intertwine/random/random.ts'
import * as tinyStyle from '@intertwine/style/style.ts'
import * as tinyWidget from '@intertwine/widget/widget.ts'

const button = tinyWidget.html.button
const div = tinyWidget.html.div
const form = tinyWidget.html.form
const input = tinyWidget.html.input
const img = tinyWidget.html.img
const range = tinyWidget.range

const column = tinyStyle.build([
  {
    marginTop: '20px',
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'flex-start',
  },
])

export const query = tinyWidget.define(
  (
    ctx: tinyWidget.Context &
      tinyError.Context &
      tinyRandom.Context
  ): {
    body: tinyWidget.Widget
  } => {
    const writeInput = input(ctx, {
      value: '',
      type: 'file',
      required: true,
      accept: 'image/*',
    })
    const resultsRange = range(ctx, {
      content: [],
    })
    const readInput = input(ctx, {
      placeholder: 'File ID',
      listen: { input: read },
    })
    const readOutput = img(ctx, {})
    const writeButton = button(ctx, {
      content: ['Write'],
    })
    const writeForm = form(ctx, {
      content: [writeInput, writeButton],
      listen: { submit: write },
    })
    const body = div(ctx, {
      styles: [column],
      content: [
        writeForm,
        resultsRange,
        readInput,
        readOutput,
      ],
    })
    return {
      body,
    }

    function write(event: Event) {
      event.preventDefault()
      resultsRange.content = [
        div(ctx, {
          content: [],
        }),
      ]
    }

    function read() {
      // Nothing
    }
  }
)

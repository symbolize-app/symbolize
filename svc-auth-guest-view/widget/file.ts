import type * as error from '@intertwine/lib-error'
import type * as random from '@intertwine/lib-random'
import * as style from '@intertwine/lib-style'
import type {} from '@intertwine/lib-time'
import * as widget from '@intertwine/lib-widget'

const button = widget.html.button
const div = widget.html.div
const form = widget.html.form
const input = widget.html.input
const img = widget.html.img
const range = widget.range

const column = style.build([
  {
    marginTop: '20px',
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'flex-start',
  },
])

export const query = widget.define(
  (
    ctx: widget.Context & error.Context & random.Context
  ): {
    body: widget.Widget
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
      content: [writeForm, resultsRange, readInput, readOutput],
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

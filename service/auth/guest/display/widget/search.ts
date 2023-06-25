import type * as tinyError from '@intertwine/error/error.ts'
import type * as tinyRandom from '@intertwine/random/random.ts'
import * as tinyStyle from '@intertwine/style/style.ts'
import * as tinyWidget from '@intertwine/widget/widget.ts'

const button = tinyWidget.html.button
const div = tinyWidget.html.div
const form = tinyWidget.html.form
const input = tinyWidget.html.input
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
    const queryInput = input(ctx, {
      name: 'query',
      value: '',
      type: 'search',
    })
    const resultsRange = range(ctx, {
      content: [],
    })
    const queryButton = button(ctx, {
      content: ['Search'],
    })
    const queryForm = form(ctx, {
      content: [queryInput, queryButton],
      listen: { submit: query },
    })
    const body = div(ctx, {
      styles: [column],
      content: [queryForm, resultsRange],
    })
    return {
      body,
    }

    function query(event: Event) {
      event.preventDefault()
      resultsRange.content = ['No results']
    }
  }
)

import type * as error from '@intertwine/lib-error'
import type * as random from '@intertwine/lib-random'
import * as style from '@intertwine/lib-style'
import * as widget from '@intertwine/lib-widget'

const button = widget.html.button
const div = widget.html.div
const form = widget.html.form
const input = widget.html.input
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

import * as appEndpointSearch from '@fe/core/endpoint/search.ts'
import * as appLanguage from '@fe/core/language.ts'
import type * as errorModule from '@tiny/core/error.ts'
import type * as random from '@tiny/core/random.ts'
import * as submit from '@tiny/core/submit.ts'
import * as style from '@tiny/ui/style.ts'
import * as widget from '@tiny/ui/widget.ts'

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
    ctx: widget.Context &
      submit.Context &
      errorModule.Context &
      random.Context
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

    async function query(event: Event) {
      event.preventDefault()
      const okResponseData = await submit.retrySubmit(
        ctx,
        'execute search',
        appEndpointSearch.query,
        {
          params: {
            language: appLanguage.Language.english,
            query: queryInput.value,
          },
        }
      )
      const { results } = okResponseData.json
      resultsRange.content = results.length
        ? results.map((result) =>
            div(ctx, {
              content: [JSON.stringify(result)],
            })
          )
        : ['No results']
    }
  }
)

import * as appEndpointSearch from '@fe/core/endpoint/search.ts'
import * as appLanguage from '@fe/core/language.ts'
import type * as tinyError from '@tiny/core/error.ts'
import type * as tinyRandom from '@tiny/core/random.ts'
import * as tinySubmit from '@tiny/core/submit.ts'
import * as tinyStyle from '@tiny/ui/style.ts'
import * as tinyWidget from '@tiny/ui/widget.ts'

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
      tinySubmit.Context &
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

    async function query(event: Event) {
      event.preventDefault()
      const okResponseData = await tinySubmit.retrySubmit(
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

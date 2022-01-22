import * as appEndpointFile from '@fe/core/endpoint/file.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinyRandom from '@tiny/core/random.ts'
import * as tinySubmit from '@tiny/core/submit.ts'
import * as tinyStyle from '@tiny/ui/style.ts'
import * as tinyWidget from '@tiny/ui/widget.ts'

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
      tinySubmit.Context &
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

    async function write(event: Event) {
      event.preventDefault()
      const okResponseData = await tinySubmit.retrySubmit(
        ctx,
        'execute search',
        appEndpointFile.write,
        {
          params: {
            requestId: tinyRandom.requestIdHex(ctx),
          },
          blob:
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            writeInput.files![0],
        }
      )
      const { id } = okResponseData.json
      resultsRange.content = [
        div(ctx, {
          content: [JSON.stringify(id)],
        }),
      ]
    }

    function read() {
      readOutput.src = tinySubmit.getEndpointUrl(
        appEndpointFile.read,
        {
          params: {
            id: readInput.value,
          },
        }
      )
    }
  }
)

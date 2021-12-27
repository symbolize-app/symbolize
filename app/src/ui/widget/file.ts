import * as appEndpointFile from '@fe/core/endpoint/file.ts'
import * as appSubmit from '@fe/core/submit.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as random from '@tiny/core/random.ts'
import type * as submit from '@tiny/core/submit.ts'
import * as style from '@tiny/ui/style.ts'
import * as widget from '@tiny/ui/widget.ts'

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

export const query = widget.define<
  {
    body: widget.Widget
  },
  submit.Context & errorModule.Context & random.Context
>((ctx) => {
  const writeInput = input(ctx, {
    name: 'query',
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
    const okResponseData = await appSubmit.retrySubmit(
      ctx,
      'execute search',
      appEndpointFile.write,
      {
        params: {
          requestId: random.requestIdHex(ctx),
        },
        stream:
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          writeInput.files![0].stream() as unknown as ReadableStream,
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
    readOutput.src = appSubmit.getUrl(
      appEndpointFile.read,
      {
        params: {
          id: readInput.value,
        },
      }
    )
  }
})

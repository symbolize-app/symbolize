import type * as tinyError from '@intertwine/error/error.ts'
import * as tinyRandom from '@intertwine/random/random.ts'
import * as tinyWidget from '@intertwine/widget/widget.ts'

const button = tinyWidget.html.button
const div = tinyWidget.html.div
const form = tinyWidget.html.form
const input = tinyWidget.html.input

export const create = tinyWidget.define(
  (
    ctx: tinyWidget.Context &
      tinyError.Context &
      tinyRandom.Context
  ): {
    body: tinyWidget.Widget
  } => {
    const requestIdInput = input(ctx, {
      name: 'requestId',
      type: 'hidden',
      value: tinyRandom.requestIdHex(ctx),
    })
    const emailInput = input(ctx, {
      name: 'email',
      value: 'a@b.com',
    })
    const handleInput = input(ctx, {
      name: 'handle',
      value: 'aaa',
    })
    const statusDiv = div(ctx, {})
    const body = form(ctx, {
      content: [
        requestIdInput,
        emailInput,
        handleInput,
        button(ctx, { content: ['Submit'] }),
        statusDiv,
      ],
      listen: {
        submit: handleSubmit,
      },
    })
    return {
      body,
    }

    function handleSubmit(event: Event) {
      event.preventDefault()
      statusDiv.content = [`Member created`]
    }
  }
)

import type * as error from '@intertwine/lib-error'
import * as random from '@intertwine/lib-random'
import * as widget from '@intertwine/lib-widget'

const button = widget.html.button
const div = widget.html.div
const form = widget.html.form
const input = widget.html.input

export const create = widget.define(
  (
    ctx: widget.Context & error.Context & random.Context
  ): {
    body: widget.Widget
  } => {
    const requestIdInput = input(ctx, {
      name: 'requestId',
      type: 'hidden',
      value: random.requestIdHex(ctx),
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

import * as appEndpoint from '@fe/core/endpoint.ts'
import * as appSubmit from '@fe/ui/submit.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as random from '@tiny/core/random.ts'
import type * as submit from '@tiny/ui/submit.ts'
import * as widget from '@tiny/ui/widget.ts'

const button = widget.html.button
const div = widget.html.div
const form = widget.html.form
const input = widget.html.input

export const custom = widget.define<
  {
    body: widget.Widget
  },
  submit.Context & errorModule.Context & random.Context
>((ctx) => {
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
  const status = div(ctx, {})
  const body = form(ctx, {
    content: [
      emailInput,
      handleInput,
      button(ctx, { content: ['Submit'] }),
      status,
    ],
    listen: {
      submit,
    },
  })
  return {
    body,
  }

  async function submit(event: Event) {
    event.preventDefault()
    try {
      const responseObject = await appSubmit.retryConflictPostSubmit(
        ctx,
        'create member',
        appEndpoint.memberCreate,
        {
          requestId: requestIdInput.value,
          email: emailInput.value,
          handle: handleInput.value,
        }
      )
      status.content = [
        `Member created ${JSON.stringify(responseObject)}`,
      ]
    } catch (error: unknown) {
      if (
        error instanceof
        appEndpoint.memberCreate.conflictError
      ) {
        status.content = [
          `Unique constraint error ${error.field}`,
        ]
        return
      }
      throw error
    }
  }
})

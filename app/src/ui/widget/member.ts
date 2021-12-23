import * as appEndpointMember from '@fe/core/endpoint/member.ts'
import * as appSubmit from '@fe/core/submit.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as random from '@tiny/core/random.ts'
import type * as submit from '@tiny/core/submit.ts'
import * as widget from '@tiny/ui/widget.ts'

const button = widget.html.button
const div = widget.html.div
const form = widget.html.form
const input = widget.html.input

export const create = widget.define<
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
      submit,
    },
  })
  return {
    body,
  }

  async function submit(event: Event) {
    event.preventDefault()
    try {
      const okResponseData =
        await appSubmit.retryConflictPostSubmit(
          ctx,
          'create member',
          appEndpointMember.create,
          {
            body: {
              requestId: requestIdInput.value,
              email: emailInput.value,
              handle: handleInput.value,
            },
          }
        )
      statusDiv.content = [
        `Member created ${JSON.stringify(okResponseData)}`,
      ]
    } catch (error) {
      if (
        error instanceof
        appEndpointMember.create.conflictResponseJson.error
      ) {
        statusDiv.content = [
          `Unique constraint error ${error.field}`,
        ]
        return
      }
      throw error
    }
  }
})

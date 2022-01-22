import * as appEndpointMember from '@app/core/endpoint/member.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinyRandom from '@tiny/core/random.ts'
import * as tinySubmit from '@tiny/core/submit.ts'
import * as tinyWidget from '@tiny/ui/widget.ts'

const button = tinyWidget.html.button
const div = tinyWidget.html.div
const form = tinyWidget.html.form
const input = tinyWidget.html.input

export const create = tinyWidget.define(
  (
    ctx: tinyWidget.Context &
      tinySubmit.Context &
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

    async function handleSubmit(event: Event) {
      event.preventDefault()
      try {
        const okResponseData = await tinySubmit.retrySubmit(
          ctx,
          'create member',
          appEndpointMember.create,
          {
            json: {
              requestId: requestIdInput.value,
              email: emailInput.value,
              handle: handleInput.value,
            },
          }
        )
        statusDiv.content = [
          `Member created ${JSON.stringify(
            okResponseData.json
          )}`,
        ]
      } catch (error) {
        if (
          error instanceof
          appEndpointMember.create.conflictResponseJson
            .error
        ) {
          statusDiv.content = [
            `Unique constraint error ${error.field}`,
          ]
          return
        }
        throw error
      }
    }
  }
)

import * as apiPayload from '@fe/api/payload.ts'
import type * as submit from '@tiny/api/submit.ts'
import * as widget from '@tiny/ui/widget.ts'
import * as errorModule from '@tiny/util/error.ts'
import * as random from '@tiny/util/random.ts'
import * as time from '@tiny/util/time.ts'
import ms from 'ms'

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
    const path = '/api/member/create'
    const method = 'POST'
    const payload: apiPayload.MemberCreateRequest = {
      requestId: requestIdInput.value,
      email: emailInput.value,
      handle: handleInput.value,
    }
    const body = JSON.stringify(
      apiPayload.checkMemberCreateRequest(payload)
    )
    try {
      const responseObject = await errorModule.retry(
        ctx,
        async () => {
          const response = await ctx.submit({
            path,
            method,
            body,
          })
          if (response.status === 409) {
            const conflictResponseObject = apiPayload.checkMemberCreateConflictResponse(
              await response.json()
            )
            throw new apiPayload.MemberCreateConflictError(
              conflictResponseObject.conflict
            )
          } else if (response.status === 200) {
            return apiPayload.checkMemberCreateResponse(
              await response.json()
            )
          } else {
            throw new Error(
              `status ${response.status} response for ${method} ${path}`
            )
          }
        },
        {
          maxAttempts: 15,
          minDelayMs: time.interval({
            milliseconds: 10,
          }),
          windowMs: time.interval({ seconds: 30 }),
          onError(error, attempt, nextDelayMs) {
            if (
              error instanceof
              apiPayload.MemberCreateConflictError
            ) {
              throw error
            }
            console.error(
              `Retrying member create (attempt ${attempt}, delay ${ms(
                nextDelayMs
              )})`,
              error
            )
          },
        }
      )
      status.content = [
        `Member created ${JSON.stringify(responseObject)}`,
      ]
    } catch (error: unknown) {
      if (
        error instanceof
        apiPayload.MemberCreateConflictError
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

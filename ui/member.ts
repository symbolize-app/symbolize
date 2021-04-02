import type * as submit from '@tiny/api/submit.ts'
import * as widget from '@tiny/ui/widget.ts'
import * as errorModule from '@tiny/util/error.ts'
import * as requestId from '@tiny/util/requestId.ts'
import * as time from '@tiny/util/time.ts'
import ms from 'ms'

const button = widget.html.button
const form = widget.html.form
const input = widget.html.input

class MemberCreateUniqueConstraintError extends Error {
  field: 'email' | 'handle'
  constructor(field: 'email' | 'handle') {
    super(`${field} conflict`)
    this.field = field
  }
}

export const custom = widget.define<
  {
    body: widget.Widget
  },
  submit.SubmitContext & errorModule.RetryContext
>((ctx) => {
  const requestIdInput = input(ctx, {
    name: 'requestId',
    type: 'hidden',
    value: requestId.generate(),
  })
  const emailInput = input(ctx, {
    name: 'email',
    value: 'a@b.com',
  })
  const handleInput = input(ctx, {
    name: 'handle',
    value: 'aaa',
  })

  const body = form(ctx, {
    content: [
      emailInput,
      handleInput,
      button(ctx, { content: ['Submit'] }),
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
    const body = JSON.stringify({
      requestId: requestIdInput.value,
      email: emailInput.value,
      handle: handleInput.value,
    })
    try {
      const responseJson = await errorModule.retry(
        ctx,
        async () => {
          const response = await ctx.submit({
            path,
            method,
            body,
          })
          if (response.status === 409) {
            const responseJson = (await response.json()) as {
              conflict: 'email' | 'handle'
            }
            throw new MemberCreateUniqueConstraintError(
              responseJson.conflict
            )
          } else if (response.status === 200) {
            return (await response.json()) as {
              id: string
            }
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
              MemberCreateUniqueConstraintError
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
      console.log('Member created', responseJson)
    } catch (error: unknown) {
      if (
        error instanceof MemberCreateUniqueConstraintError
      ) {
        console.error(
          `Unique constraint error ${error.field}`
        )
        return
      }
      throw error
    }
  }
})
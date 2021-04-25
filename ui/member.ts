import * as apiPayload from '@fe/api/payload.ts'
import type * as payload from '@tiny/api/payload.ts'
import type * as submit from '@tiny/api/submit.ts'
import * as errorModule from '@tiny/core/error.ts'
import * as random from '@tiny/core/random.ts'
import * as time from '@tiny/core/time.ts'
import * as widget from '@tiny/ui/widget.ts'
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
    try {
      const responseObject = await retryConflictPostSubmit(
        ctx,
        'create member',
        apiPayload.checkMemberCreateRequest,
        apiPayload.checkMemberCreateOkResponse,
        apiPayload.MemberCreateConflictError,
        apiPayload.checkMemberCreateConflictResponse,
        '/api/member/create',
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
function postSubmit<Request>(
  ctx: errorModule.Context & submit.Context,
  checkRequest: payload.Validator<Request>,
  path: string,
  requestObject: Request
): () => Promise<submit.Response> {
  const method = 'POST'
  const headers = { 'content-type': 'application/json' }
  const body = checkRequest(requestObject)
  return () =>
    ctx.submit({
      path,
      method,
      headers,
      body,
    })
}

async function retrySubmit<OkResponse>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  checkOkResponse: payload.Validator<OkResponse>,
  submit: () => Promise<submit.Response>,
  onError?: (error: unknown) => void
): Promise<OkResponse> {
  return await errorModule.retry(
    ctx,
    async () => {
      const response = await submit()
      if (response.status === 200) {
        return checkOkResponse(await response.json())
      } else {
        throw new Error(
          `Unexpected status ${response.status} response during ${description}`
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
        if (onError) {
          onError(error)
        }
        console.error(
          `Retrying ${description} submit (attempt ${attempt}, delay ${ms(
            nextDelayMs
          )})`,
          error
        )
      },
    }
  )
}

function retryConflictSubmit<
  OkResponse,
  ConflictResponse extends { conflict: string }
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  checkOkResponse: payload.Validator<OkResponse>,
  ConflictError: new (
    field: ConflictResponse['conflict']
  ) => payload.ConflictError<ConflictResponse>,
  checkConflictResponse: payload.Validator<ConflictResponse>,
  submit: () => Promise<submit.Response>
): Promise<OkResponse> {
  return retrySubmit(
    ctx,
    description,
    checkOkResponse,
    async () => {
      const response = await submit()
      if (response.status === 409) {
        const conflictResponseObject = checkConflictResponse(
          await response.json()
        )
        throw new ConflictError(
          conflictResponseObject.conflict
        )
      } else {
        return response
      }
    },
    (error: unknown) => {
      if (error instanceof ConflictError) {
        throw error
      }
    }
  )
}

function retryConflictPostSubmit<
  Request,
  OkResponse,
  ConflictResponse extends { conflict: string }
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  checkRequest: payload.Validator<Request>,
  checkOkResponse: payload.Validator<OkResponse>,
  ConflictError: new (
    field: ConflictResponse['conflict']
  ) => payload.ConflictError<ConflictResponse>,
  checkConflictResponse: payload.Validator<ConflictResponse>,
  path: string,
  requestObject: Request
): Promise<OkResponse> {
  return retryConflictSubmit(
    ctx,
    description,
    checkOkResponse,
    ConflictError,
    checkConflictResponse,
    postSubmit(ctx, checkRequest, path, requestObject)
  )
}

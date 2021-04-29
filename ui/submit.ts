import type * as endpoint from '@tiny/core/endpoint.ts'
import * as errorModule from '@tiny/core/error.ts'
import type * as payload from '@tiny/core/payload.ts'
import * as time from '@tiny/core/time.ts'
import type * as submit from '@tiny/ui/submit.ts'
import ms from 'ms'

export function retryGetSubmit<
  Request extends Record<string, string>,
  OkResponse
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: endpoint.GetEndpoint<Request, OkResponse>,
  requestData: Request
): Promise<OkResponse> {
  return retrySubmit(
    ctx,
    description,
    endpoint,
    submitParams(ctx, endpoint, requestData)
  )
}

export function retryConflictPostSubmit<
  Request,
  OkResponse,
  ConflictResponse extends { conflict: string }
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: endpoint.ConflictPostEndpoint<
    Request,
    OkResponse,
    ConflictResponse
  >,
  requestData: Request
): Promise<OkResponse> {
  return retryConflictSubmit(
    ctx,
    description,
    endpoint,
    submitJson(ctx, endpoint, requestData)
  )
}

function submitParams<
  Request extends Record<string, string>
>(
  ctx: errorModule.Context & submit.Context,
  endpoint: endpoint.BaseEndpoint<'GET'> & {
    checkRequest: payload.Validator<Request>
  },
  requestData: Request
): () => Promise<submit.Response> {
  const params = endpoint.checkRequest(requestData)
  return () =>
    ctx.submit({
      path: endpoint.path,
      method: endpoint.method,
      params,
    })
}

function submitJson<Request>(
  ctx: errorModule.Context & submit.Context,
  endpoint: endpoint.BaseEndpoint<'POST'> & {
    checkRequest: payload.Validator<Request>
  },
  requestData: Request
): () => Promise<submit.Response> {
  const headers = { 'content-type': 'application/json' }
  const body = endpoint.checkRequest(requestData)
  return () =>
    ctx.submit({
      path: endpoint.path,
      method: endpoint.method,
      headers,
      body,
    })
}

async function retrySubmit<OkResponse>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: {
    checkOkResponse: payload.Validator<OkResponse>
  },
  submit: () => Promise<submit.Response>,
  onError?: (error: unknown) => void
): Promise<OkResponse> {
  return await errorModule.retry(
    ctx,
    async () => {
      const response = await submit()
      if (response.status === 200) {
        return endpoint.checkOkResponse(
          await response.json()
        )
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
  endpoint: {
    checkOkResponse: payload.Validator<OkResponse>
    conflictError: new (
      field: ConflictResponse['conflict']
    ) => payload.ConflictError<ConflictResponse>
    checkConflictResponse: payload.Validator<ConflictResponse>
  },
  submit: () => Promise<submit.Response>
): Promise<OkResponse> {
  return retrySubmit(
    ctx,
    description,
    endpoint,
    async () => {
      const response = await submit()
      if (response.status === 409) {
        const conflictResponseData = endpoint.checkConflictResponse(
          await response.json()
        )
        throw new endpoint.conflictError(
          conflictResponseData.conflict
        )
      } else {
        return response
      }
    },
    (error: unknown) => {
      if (error instanceof endpoint.conflictError) {
        throw error
      }
    }
  )
}

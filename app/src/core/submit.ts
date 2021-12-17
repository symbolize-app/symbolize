import type * as endpoint from '@tiny/core/endpoint.ts'
import * as errorModule from '@tiny/core/error.ts'
import type * as payload from '@tiny/core/payload.ts'
import * as time from '@tiny/core/time.ts'
import type * as submit from '@tiny/core/submit.ts'
import ms from 'ms'

export function retryGetSubmit<
  RequestData extends Record<string, string>,
  OkResponseData
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: endpoint.GetEndpoint<
    RequestData,
    OkResponseData
  >,
  request: Pick<submit.Request, 'origin' | 'headers'> & {
    params: RequestData
  }
): Promise<OkResponseData> {
  return retryBaseSubmit(ctx, description, endpoint, {
    origin: request.origin,
    path: endpoint.path,
    params: endpoint.checkRequest(request.params),
    method: endpoint.method,
    headers: request.headers,
  })
}

export function retryConflictPostSubmit<
  RequestData,
  OkResponseData,
  ConflictResponseData extends { conflict: string }
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: endpoint.ConflictPostEndpoint<
    RequestData,
    OkResponseData,
    ConflictResponseData
  >,
  request: Pick<submit.Request, 'origin' | 'headers'> & {
    body: RequestData
  }
): Promise<OkResponseData> {
  return retryConflictSubmit(ctx, description, endpoint, {
    origin: request.origin,
    path: endpoint.path,
    method: endpoint.method,
    headers: {
      'content-type': 'application/json',
      ...request.headers,
    },
    body: endpoint.checkRequest(request.body),
  })
}

async function retryBaseSubmit<OkResponseData>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: {
    checkOkResponse: payload.Validator<OkResponseData>
  },
  request: submit.Request,
  onResponse?: (response: submit.Response) => Promise<void>,
  onError?: (error: unknown) => void
): Promise<OkResponseData> {
  return await errorModule.retry(
    ctx,
    async () => {
      const response = await ctx.submit(request)
      if (onResponse) {
        await onResponse(response)
      }
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
  OkResponseData,
  ConflictResponseData extends { conflict: string }
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: {
    checkOkResponse: payload.Validator<OkResponseData>
    conflictError: new (
      field: ConflictResponseData['conflict']
    ) => payload.ConflictError<ConflictResponseData>
    checkConflictResponse: payload.Validator<ConflictResponseData>
  },
  request: submit.Request
): Promise<OkResponseData> {
  return retryBaseSubmit(
    ctx,
    description,
    endpoint,
    request,
    async (response) => {
      if (response.status === 409) {
        const conflictResponseData = endpoint.checkConflictResponse(
          await response.json()
        )
        throw new endpoint.conflictError(
          conflictResponseData.conflict
        )
      }
    },
    (error) => {
      if (error instanceof endpoint.conflictError) {
        throw error
      }
    }
  )
}

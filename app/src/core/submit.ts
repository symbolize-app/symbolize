import type * as endpoint from '@tiny/core/endpoint.ts'
import * as errorModule from '@tiny/core/error.ts'
import type * as payload from '@tiny/core/payload.ts'
import type * as submit from '@tiny/core/submit.ts'
import * as time from '@tiny/core/time.ts'
import ms from 'ms'
import type * as typeFest from 'type-fest'

export function retryGetSubmit<
  Endpoint extends endpoint.GetEndpoint
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: Endpoint,
  request: Pick<submit.Request, 'origin' | 'headers'> & {
    params: endpoint.RequestData<Endpoint>
  }
): Promise<endpoint.OkResponseData<Endpoint>> {
  return retryBaseSubmit(ctx, description, endpoint, {
    origin: request.origin,
    path: endpoint.path,
    params: endpoint.checkRequest(request.params),
    method: endpoint.method,
    headers: request.headers,
  })
}

export function retryConflictPostSubmit<
  Endpoint extends endpoint.ConflictPostEndpoint
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: Endpoint,
  request: Pick<submit.Request, 'origin' | 'headers'> & {
    body: endpoint.RequestData<Endpoint>
  }
): Promise<endpoint.OkResponseData<Endpoint>> {
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

async function retryBaseSubmit<
  Endpoint extends {
    checkOkResponse: payload.Validator<typeFest.JsonObject>
  }
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: Endpoint,
  request: submit.Request,
  onResponse?: (response: submit.Response) => Promise<void>,
  onError?: (error: unknown) => void
): Promise<endpoint.OkResponseData<Endpoint>> {
  return (await errorModule.retry(
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
  )) as endpoint.OkResponseData<Endpoint>
}

function retryConflictSubmit<
  Endpoint extends {
    checkOkResponse: payload.Validator<typeFest.JsonObject>
    conflictError: new (
      field: string
    ) => payload.ConflictError<
      typeFest.JsonObject & {
        conflict: string
      }
    >
    checkConflictResponse: payload.Validator<
      typeFest.JsonObject & {
        conflict: string
      }
    >
  }
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: Endpoint,
  request: submit.Request
): Promise<endpoint.OkResponseData<Endpoint>> {
  return retryBaseSubmit(
    ctx,
    description,
    endpoint,
    request,
    async (response) => {
      if (response.status === 409) {
        const conflictResponseData =
          endpoint.checkConflictResponse(
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

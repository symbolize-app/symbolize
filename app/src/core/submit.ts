import type * as endpoint from '@tiny/core/endpoint.ts'
import * as errorModule from '@tiny/core/error.ts'
import type * as payload from '@tiny/core/payload.ts'
import type * as submit from '@tiny/core/submit.ts'
import * as time from '@tiny/core/time.ts'
import ms from 'ms'

export function retryGetJsonSubmit<
  Endpoint extends endpoint.GetEndpoint<{
    requestParams: payload.Validator<Record<string, string>>
    okResponseJson: payload.Validator
  }>
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: Endpoint,
  request: Pick<submit.Request, 'origin' | 'headers'> & {
    params: payload.Payload<Endpoint["requestParams"]>
  }
): Promise<payload.Payload<Endpoint["okResponseJson"]>> {
  return retryBaseSubmit(ctx, description, endpoint, {
    origin: request.origin,
    path: endpoint.path,
    params: endpoint.requestParams.check(request.params),
    method: endpoint.method,
    headers: request.headers,
  })
}

export function retryConflictPostSubmit<
  Endpoint extends endpoint.PostEndpoint<{
    requestJson: payload.Validator
    okResponseJson: payload.Validator
    conflictResponseJson: payload.ConflictValidator
  }>
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: Endpoint,
  request: Pick<submit.Request, 'origin' | 'headers'> & {
    body: payload.Payload<Endpoint["requestJson"]>
  }
): Promise<payload.Payload<Endpoint["okResponseJson"]>> {
  return retryConflictSubmit(ctx, description, endpoint, {
    origin: request.origin,
    path: endpoint.path,
    method: endpoint.method,
    headers: {
      'content-type': 'application/json',
      ...request.headers,
    },
    body: endpoint.requestJson.check(request.body),
  })
}

async function retryBaseSubmit<
  Endpoint extends {
    okResponseJson: payload.Validator
  }
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: Endpoint,
  request: submit.Request,
  onResponse?: (response: submit.Response) => Promise<void>,
  onError?: (error: unknown) => void
): Promise<payload.Payload<Endpoint["okResponseJson"]>> {
  return (await errorModule.retry(
    ctx,
    async () => {
      const response = await ctx.submit(request)
      if (onResponse) {
        await onResponse(response)
      }
      if (response.status === 200) {
        return endpoint.okResponseJson.check(
          await response.json()
        ) as payload.Payload<Endpoint["okResponseJson"]>
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
  ))
}

function retryConflictSubmit<
  Endpoint extends {
    okResponseJson: payload.Validator
    conflictResponseJson: payload.ConflictValidator
  }
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: Endpoint,
  request: submit.Request
): Promise<payload.Payload<Endpoint["okResponseJson"]>> {
  return retryBaseSubmit(
    ctx,
    description,
    endpoint,
    request,
    async (response) => {
      if (response.status === 409) {
        const conflictResponseData =
          endpoint.conflictResponseJson.check(
            await response.json()
          )
        throw new endpoint.conflictResponseJson.error(
          conflictResponseData.conflict as never
        )
      }
    },
    (error) => {
      if (error instanceof endpoint.conflictResponseJson.error) {
        throw error
      }
    }
  )
}

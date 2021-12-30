import type * as endpoint from '@tiny/core/endpoint.ts'
import * as errorModule from '@tiny/core/error.ts'
import type * as payload from '@tiny/core/payload.ts'
import * as submit from '@tiny/core/submit.ts'
import * as time from '@tiny/core/time.ts'
import ms from 'ms'
import type * as typeFest from 'type-fest'

export function retrySubmit<
  Endpoint extends endpoint.BaseEndpoint
>(
  ctx: errorModule.Context & submit.Context,
  description: string,
  endpoint: Endpoint,
  request: Pick<submit.Request, 'origin' | 'headers'> &
    (Endpoint extends endpoint.RequestParamsEndpoint
      ? {
          params: payload.Payload<Endpoint['requestParams']>
        }
      : { params?: never }) &
    (Endpoint extends endpoint.RequestBytesEndpoint
      ? {
          stream?: ReadableStream
          blob?: Blob
          buffer?: ArrayBuffer
        }
      : { stream?: never; blob?: never; buffer?: never }) &
    (Endpoint extends endpoint.RequestJsonEndpoint
      ? {
          json: payload.Payload<Endpoint['requestJson']>
        }
      : { json?: never })
): Promise<
  (Endpoint extends endpoint.OkResponseStreamEndpoint
    ? {
        stream: ReadableStream
      }
    : { stream?: never }) &
    (Endpoint extends endpoint.OkResponseJsonEndpoint
      ? {
          json: payload.Payload<Endpoint['okResponseJson']>
        }
      : { json?: never })
> {
  const checkedRequest = createRequest(endpoint, request)
  return errorModule.retry(
    ctx,
    async () => {
      const response = await ctx.submit(checkedRequest)
      if (endpoint.conflictResponseJson) {
        if (response.status === 409) {
          const conflictResponseData =
            endpoint.conflictResponseJson.check(
              await response.json()
            )
          throw new endpoint.conflictResponseJson.error(
            conflictResponseData.conflict as never
          )
        }
      }
      if (response.status === 200) {
        return {
          ...(endpoint.okResponseStream !== undefined && {
            stream: response.stream(),
          }),
          ...(endpoint.okResponseJson !== undefined && {
            json: endpoint.okResponseJson.check(
              await response.json()
            ),
          }),
        } as (Endpoint extends endpoint.OkResponseStreamEndpoint
          ? {
              stream: ReadableStream
            }
          : { stream?: never }) &
          (Endpoint extends endpoint.OkResponseJsonEndpoint
            ? {
                json: payload.Payload<
                  Endpoint['okResponseJson']
                >
              }
            : { json?: never })
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
        if (endpoint.conflictResponseJson) {
          if (
            error instanceof
            endpoint.conflictResponseJson.error
          ) {
            throw error
          }
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

export function getUrl<
  Endpoint extends endpoint.BaseEndpoint
>(
  endpoint: Endpoint,
  request: Pick<submit.Request, 'origin' | 'headers'> &
    (Endpoint extends endpoint.RequestParamsEndpoint
      ? {
          params: payload.Payload<Endpoint['requestParams']>
        }
      : { params?: never }) &
    (Endpoint extends endpoint.RequestBytesEndpoint
      ? {
          stream?: ReadableStream
          blob?: Blob
          buffer?: ArrayBuffer
        }
      : { stream?: never; blob?: never; buffer?: never }) &
    (Endpoint extends endpoint.RequestJsonEndpoint
      ? {
          json: payload.Payload<Endpoint['requestJson']>
        }
      : { json?: never })
): string {
  return submit.getUrl(createRequest(endpoint, request))
}

export function createRequest<
  Endpoint extends endpoint.BaseEndpoint
>(
  endpoint: Endpoint,
  request: Pick<submit.Request, 'origin' | 'headers'> &
    (Endpoint extends endpoint.RequestParamsEndpoint
      ? {
          params: payload.Payload<Endpoint['requestParams']>
        }
      : { params?: never }) &
    (Endpoint extends endpoint.RequestBytesEndpoint
      ? {
          stream?: ReadableStream
          blob?: Blob
          buffer?: ArrayBuffer
        }
      : { stream?: never; blob?: never; buffer?: never }) &
    (Endpoint extends endpoint.RequestJsonEndpoint
      ? {
          json: payload.Payload<Endpoint['requestJson']>
        }
      : { json?: never })
): submit.Request {
  return {
    ...(request.origin !== undefined && {
      origin: request.origin,
    }),
    path: endpoint.path,
    method: endpoint.method,
    headers: {
      ...(endpoint.requestJson !== undefined && {
        ['content-type']: 'application/json',
      }),
      ...request.headers,
    },
    ...(endpoint.requestParams !== undefined && {
      params: endpoint.requestParams.check(
        request.params as unknown as Record<string, string>
      ),
    }),
    ...(endpoint.requestBytes !== undefined && {
      stream: request.stream,
      blob: request.blob,
      buffer: request.buffer,
    }),
    ...(endpoint.requestJson !== undefined && {
      json: endpoint.requestJson.check(
        request.json as unknown as typeFest.JsonValue
      ),
    }),
  }
}

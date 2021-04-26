import * as route from '@tiny/api/route.ts'
import * as errorModule from '@tiny/core/error.ts'
import * as payload from '@tiny/core/payload.ts'
import * as time from '@tiny/core/time.ts'
import * as query from '@tiny/db/query.ts'
import ms from 'ms'
import type * as typeFest from 'type-fest'

export async function checkRequestJson<
  Request extends typeFest.JsonObject
>(
  endpoint: { checkRequest: payload.Validator<Request> },
  request: route.Request
): Promise<Request> {
  if (
    request.headers['content-type'] !== 'application/json'
  ) {
    throw new route.ResponseError({
      status: 400,
      headers: {
        'content-type': 'application/json',
      },
      body: {
        error: 'content-type: application/json required',
      },
    })
  }
  let input: typeFest.JsonValue
  try {
    input = await request.json()
  } catch (error: unknown) {
    if (error instanceof SyntaxError) {
      throw new route.ResponseError({
        status: 400,
        headers: {
          'content-type': 'application/json',
        },
        body: {
          error: 'JSON syntax error',
        },
      })
    } else {
      throw error
    }
  }
  return checkRequestBase(endpoint, input)
}

function checkRequestBase<Value>(
  endpoint: { checkRequest: payload.Validator<Value> },
  input: typeFest.JsonValue
): Value {
  try {
    return endpoint.checkRequest(input)
  } catch (error: unknown) {
    if (error instanceof payload.PayloadError) {
      throw new route.ResponseError({
        status: 400,
        headers: {
          'content-type': 'application/json',
        },
        body: {
          error: error.message,
        },
      })
    } else {
      throw error
    }
  }
}

export async function retryQuery<Value>(
  ctx: errorModule.Context,
  description: string,
  query: () => Promise<Value>,
  onError?: (error: unknown) => void
): Promise<Value> {
  return await errorModule.retry(ctx, query, {
    maxAttempts: 15,
    minDelayMs: time.interval({ milliseconds: 10 }),
    windowMs: time.interval({ seconds: 30 }),
    onError(error, attempt, nextDelayMs) {
      if (onError) {
        onError(error)
      }
      console.error(
        `Retrying ${description} query (attempt ${attempt}, delay ${ms(
          nextDelayMs
        )})`,
        error
      )
    },
  })
}

export async function retryConflictQuery<
  Value,
  ConflictResponse extends { conflict: string }
>(
  ctx: errorModule.Context,
  description: string,
  endpoint: {
    conflictError: new (
      field: ConflictResponse['conflict']
    ) => payload.ConflictError<ConflictResponse>
    checkConflictResponse: payload.Validator<ConflictResponse>
  },
  conflictMap: Record<
    string,
    ConflictResponse['conflict'] | undefined
  >,
  queryCallback: () => Promise<Value>
): Promise<Value> {
  return await checkConflictQuery(endpoint, () =>
    retryQuery(ctx, description, queryCallback, (error) => {
      if (
        query.isQueryError(error) &&
        error.code === query.errorCode.uniqueViolation
      ) {
        const constraintName = query.getUniqueViolationConstraintName(
          error
        )
        const conflictField =
          constraintName && conflictMap[constraintName]
        if (conflictField) {
          throw new endpoint.conflictError(conflictField)
        }
      }
    })
  )
}

async function checkConflictQuery<
  Value,
  ConflictResponse extends { conflict: string }
>(
  endpoint: {
    conflictError: new (
      field: ConflictResponse['conflict']
    ) => payload.ConflictError<ConflictResponse>
    checkConflictResponse: payload.Validator<ConflictResponse>
  },
  query: () => Promise<Value>
): Promise<Value> {
  try {
    return await query()
  } catch (error: unknown) {
    if (error instanceof endpoint.conflictError) {
      throw createConflictResponseError(
        endpoint,
        error.field
      )
    } else {
      throw error
    }
  }
}

function createConflictResponseError<
  ConflictResponse extends { conflict: string }
>(
  endpoint: {
    checkConflictResponse: payload.Validator<ConflictResponse>
  },
  field: ConflictResponse['conflict']
): route.ResponseError {
  return new route.ResponseError({
    status: 409,
    headers: {
      'content-type': 'application/json',
    },
    body: endpoint.checkConflictResponse({
      conflict: field,
    }),
  })
}

export function checkOkResponse<OkResponse>(
  endpoint: {
    checkOkResponse: payload.Validator<OkResponse>
  },
  responseObject: OkResponse
): route.Response {
  return {
    status: 200,
    headers: {
      'content-type': 'application/json',
    },
    body: endpoint.checkOkResponse(responseObject),
  }
}

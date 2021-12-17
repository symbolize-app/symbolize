import * as route from '@tiny/api/route.ts'
import * as errorModule from '@tiny/core/error.ts'
import * as payload from '@tiny/core/payload.ts'
import * as time from '@tiny/core/time.ts'
import * as query from '@tiny/db/query.ts'
import ms from 'ms'
import type * as typeFest from 'type-fest'

export function checkRequestParams<
  Request extends Record<string, string>
>(
  endpoint: {
    method: 'GET'
    checkRequest: payload.Validator<Request>
  },
  request: route.Request
): Request {
  const input = request.params
  return checkRequestBase(endpoint, input)
}

export async function checkRequestJson<
  Request extends typeFest.JsonObject
>(
  endpoint: {
    method: 'POST'
    checkRequest: payload.Validator<Request>
  },
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

export async function retryQuery<
  Id extends unknown,
  Values extends query.SupportedType[],
  Row extends Record<string, query.SupportedType>,
  Transform extends unknown
>(
  ctx: errorModule.Context,
  database: query.Database<Id>,
  description: string,
  query_: query.Query<Id, Values, Row, Transform>,
  ...values: Values
): Promise<Transform> {
  return await retryBaseQuery(
    ctx,
    database,
    description,
    query_,
    undefined,
    ...values
  )
}

export async function retryConflictQuery<
  Id extends unknown,
  Values extends query.SupportedType[],
  Row extends Record<string, query.SupportedType>,
  Transform extends unknown,
  ConflictResponse extends { conflict: string }
>(
  ctx: errorModule.Context,
  database: query.Database<Id>,
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
  query_: query.Query<Id, Values, Row, Transform>,
  ...values: Values
): Promise<Transform> {
  return await checkConflictQuery(endpoint, () =>
    retryBaseQuery(
      ctx,
      database,
      description,
      query_,
      (error) => {
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
      },
      ...values
    )
  )
}

async function retryBaseQuery<
  Id extends unknown,
  Values extends query.SupportedType[],
  Row extends Record<string, query.SupportedType>,
  Transform extends unknown
>(
  ctx: errorModule.Context,
  database: query.Database<Id>,
  description: string,
  query_: query.Query<Id, Values, Row, Transform>,
  onError: ((error: unknown) => void) | undefined,
  ...values: Values
): Promise<Transform> {
  return await errorModule.retry(
    ctx,
    () => database.query(query_, ...values),
    {
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
    }
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
  okResponseData: OkResponse
): route.Response {
  return {
    status: 200,
    headers: {
      'content-type': 'application/json',
    },
    body: endpoint.checkOkResponse(okResponseData),
  }
}

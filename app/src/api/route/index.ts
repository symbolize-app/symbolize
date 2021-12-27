import * as route from '@tiny/api/route.ts'
import * as errorModule from '@tiny/core/error.ts'
import * as payload from '@tiny/core/payload.ts'
import * as time from '@tiny/core/time.ts'
import * as dbQuery from '@tiny/db/query.ts'
import ms from 'ms'
import type * as typeFest from 'type-fest'

export function checkRequestParams<
  Value extends Record<string, string>
>(
  endpoint: {
    requestParams: payload.Validator<Value>
  },
  request: route.Request
): Value {
  const input = request.params
  return checkRequestBase(endpoint.requestParams, input)
}

export async function checkRequestJson<
  Value extends typeFest.JsonObject
>(
  endpoint: {
    requestJson: payload.Validator<Value>
  },
  request: route.Request
): Promise<Value> {
  if (
    request.headers['content-type'] !== 'application/json'
  ) {
    throw new route.ResponseError({
      status: 400,
      headers: {
        'content-type': 'application/json',
      },
      json: {
        error: 'content-type: application/json required',
      },
    })
  }
  let input: typeFest.JsonValue
  try {
    input = await request.json()
  } catch (error) {
    if (error instanceof SyntaxError) {
      throw new route.ResponseError({
        status: 400,
        headers: {
          'content-type': 'application/json',
        },
        json: {
          error: 'JSON syntax error',
        },
      })
    } else {
      throw error
    }
  }
  return checkRequestBase(endpoint.requestJson, input)
}

function checkRequestBase<Value>(
  validator: payload.Validator<Value>,
  input: typeFest.JsonValue
): Value {
  try {
    return validator.check(input)
  } catch (error) {
    if (error instanceof payload.PayloadError) {
      throw new route.ResponseError({
        status: 400,
        headers: {
          'content-type': 'application/json',
        },
        json: {
          error: error.message,
        },
      })
    } else {
      throw error
    }
  }
}

export async function retryDbQuery<
  Id,
  Values extends dbQuery.SupportedType[],
  Row extends Record<string, dbQuery.SupportedType>,
  Transform
>(
  ctx: errorModule.Context,
  database: dbQuery.Database<Id>,
  description: string,
  query: dbQuery.Query<Id, Values, Row, Transform>,
  ...values: Values
): Promise<Transform> {
  return await retryDbBaseQuery(
    ctx,
    database,
    description,
    query,
    undefined,
    ...values
  )
}

export async function retryDbConflictQuery<
  Id,
  Values extends dbQuery.SupportedType[],
  Row extends Record<string, dbQuery.SupportedType>,
  Transform,
  ConflictResponse extends { conflict: string }
>(
  ctx: errorModule.Context,
  database: dbQuery.Database<Id>,
  description: string,
  endpoint: {
    conflictResponseJson: payload.ConflictValidator
  },
  conflictMap: Record<
    string,
    ConflictResponse['conflict'] | undefined
  >,
  query: dbQuery.Query<Id, Values, Row, Transform>,
  ...values: Values
): Promise<Transform> {
  // TODO Put conflict map into query, merge with retry DB query, move to app/db
  return await checkConflictQuery(endpoint, () =>
    retryDbBaseQuery(
      ctx,
      database,
      description,
      query,
      (error) => {
        if (
          dbQuery.isQueryError(error) &&
          error.code === dbQuery.errorCode.uniqueViolation
        ) {
          const constraintName =
            dbQuery.getUniqueViolationConstraintName(error)
          const conflictField =
            constraintName && conflictMap[constraintName]
          if (conflictField) {
            throw new endpoint.conflictResponseJson.error(
              conflictField as never
            )
          }
        }
      },
      ...values
    )
  )
}

async function retryDbBaseQuery<
  Id,
  Values extends dbQuery.SupportedType[],
  Row extends Record<string, dbQuery.SupportedType>,
  Transform
>(
  ctx: errorModule.Context,
  database: dbQuery.Database<Id>,
  description: string,
  query: dbQuery.Query<Id, Values, Row, Transform>,
  onError: ((error: unknown) => void) | undefined,
  ...values: Values
): Promise<Transform> {
  return await errorModule.retry(
    ctx,
    () => database.query(query, ...values),
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
  Field extends string
>(
  endpoint: {
    conflictResponseJson: payload.ConflictValidator<Field>
  },
  query: () => Promise<Value>
): Promise<Value> {
  try {
    return await query()
  } catch (error) {
    if (
      error instanceof endpoint.conflictResponseJson.error
    ) {
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
  Endpoint extends {
    conflictResponseJson: payload.ConflictValidator
  }
>(
  endpoint: Endpoint,
  field: payload.Payload<
    Endpoint['conflictResponseJson']
  >['conflict']
): route.ResponseError {
  return new route.ResponseError({
    status: 409,
    headers: {
      'content-type': 'application/json',
    },
    json: endpoint.conflictResponseJson.check({
      conflict: field,
    }),
  })
}

export function checkOkResponse<
  Endpoint extends {
    okResponseJson: payload.Validator<typeFest.JsonObject>
  }
>(
  endpoint: Endpoint,
  okResponseData: payload.Payload<
    Endpoint['okResponseJson']
  >
): route.Response {
  // TODO Put response in JSON
  return {
    status: 200,
    headers: {
      'content-type': 'application/json',
    },
    json: endpoint.okResponseJson.check(okResponseData),
  }
}

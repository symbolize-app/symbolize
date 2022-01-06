import * as route from '@tiny/api/route.ts'
import * as payload from '@tiny/core/payload.ts'
import type * as typeFest from 'type-fest'

export function initContext(): route.Context {
  return {
    maxRequestNonStreamedBytes: 4 * 1024,
  }
}

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

export async function checkConflictQuery<
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

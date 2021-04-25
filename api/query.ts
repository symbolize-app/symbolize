import * as message from '@fe/core/message.ts'
import * as apiPayload from '@fe/core/payload.ts'
import type * as db from '@fe/db/index.ts'
import * as memberQuery from '@fe/db/query/member.ts'
import * as route from '@tiny/api/route.ts'
import * as crypto from '@tiny/core/crypto.node.ts'
import * as errorModule from '@tiny/core/error.ts'
import * as payload from '@tiny/core/payload.ts'
import * as time from '@tiny/core/time.ts'
import * as query from '@tiny/db/query.ts'
import ms from 'ms'
import type * as typeFest from 'type-fest'

const apiMessage = route.define<
  errorModule.Context & db.ReadContext
>(['GET'], /^\/api\/message$/, async (ctx) => {
  const row = await retryQuery(ctx, 'member find', () =>
    ctx.databaseApiRead.query(
      memberQuery.find,
      Buffer.from('ABCD', 'hex')
    )
  )
  return {
    status: 200,
    headers: {
      'content-type': 'text/plain',
    },
    body: `${message.hi} ${JSON.stringify(row)}`,
  }
})

export const apiMemberCreate = route.define<
  errorModule.Context & db.WriteContext
>(
  ['POST'],
  /^\/api\/member\/create$/,
  async (ctx, request) => {
    const requestObject = await checkRequestJson(
      apiPayload.checkMemberCreateRequest,
      request
    )
    const id = crypto.hash(
      Buffer.from(requestObject.requestId, 'hex')
    )
    await retryConflictQuery(
      ctx,
      'member create',
      apiPayload.MemberCreateConflictError,
      apiPayload.checkMemberCreateConflictResponse,
      {
        ['member_email_key']: 'email',
        ['member_handle_key']: 'handle',
      },
      () =>
        ctx.databaseApiWrite.query(
          memberQuery.create,
          id,
          requestObject.email,
          requestObject.handle
        )
    )
    return checkOkResponse(
      apiPayload.checkMemberCreateOkResponse,
      {
        id: id.toString('hex'),
      }
    )
  }
)

export const routes = [apiMessage, apiMemberCreate]

function checkRequest<Value>(
  check: payload.Validator<Value>,
  input: typeFest.JsonValue
) {
  try {
    return check(input)
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

async function checkRequestJson<Value>(
  check: payload.Validator<Value>,
  request: route.Request
) {
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
  return checkRequest(check, input)
}

async function checkConflictQuery<
  Value,
  ConflictResponse extends { conflict: string }
>(
  conflictError: new (
    field: ConflictResponse['conflict']
  ) => payload.ConflictError<ConflictResponse>,
  check: payload.Validator<ConflictResponse>,
  query: () => Promise<Value>
): Promise<Value> {
  try {
    return await query()
  } catch (error: unknown) {
    if (error instanceof conflictError) {
      throw checkConflictResponse(check, error.field)
    } else {
      throw error
    }
  }
}

function checkConflictResponse<
  ConflictResponse extends { conflict: string }
>(
  check: payload.Validator<ConflictResponse>,
  field: ConflictResponse['conflict']
): route.ResponseError {
  return new route.ResponseError({
    status: 409,
    headers: {
      'content-type': 'application/json',
    },
    body: check({
      conflict: field,
    }),
  })
}

async function retryQuery<Value>(
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

async function retryConflictQuery<
  Value,
  ConflictResponse extends { conflict: string }
>(
  ctx: errorModule.Context,
  description: string,
  conflictError: new (
    field: ConflictResponse['conflict']
  ) => payload.ConflictError<ConflictResponse>,
  checkConflictResponse: payload.Validator<ConflictResponse>,
  conflictMap: Record<
    string,
    ConflictResponse['conflict'] | undefined
  >,
  queryCallback: () => Promise<Value>
): Promise<Value> {
  return await checkConflictQuery(
    conflictError,
    checkConflictResponse,
    () =>
      retryQuery(
        ctx,
        description,
        queryCallback,
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
              throw new conflictError(conflictField)
            }
          }
        }
      )
  )
}

function checkOkResponse<Value>(
  check: payload.Validator<Value>,
  responseObject: Value
): route.Response {
  return {
    status: 200,
    headers: {
      'content-type': 'application/json',
    },
    body: check(responseObject),
  }
}

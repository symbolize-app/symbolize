import * as apiPayload from '@fe/api/payload.ts'
import * as message from '@fe/core/message.ts'
import type * as db from '@fe/db/index.ts'
import * as memberQuery from '@fe/db/query/member.ts'
import * as payload from '@tiny/api/payload.ts'
import * as route from '@tiny/api/route.ts'
import * as query from '@tiny/db/query.ts'
import * as crypto from '@tiny/util/crypto.ts'
import * as errorModule from '@tiny/util/error.ts'
import * as time from '@tiny/util/time.ts'
import ms from 'ms'

const apiMessage = route.define<
  errorModule.Context & db.ReadContext
>(['GET'], /^\/api\/message$/, async (ctx) => {
  const row = await errorModule.retry(
    ctx,
    () =>
      memberQuery.find(
        ctx.databaseApiRead,
        Buffer.from('ABCD', 'hex')
      ),
    {
      maxAttempts: 15,
      minDelayMs: time.interval({ milliseconds: 10 }),
      windowMs: time.interval({ seconds: 30 }),
      onError(error, attempt, nextDelayMs) {
        console.error(
          `Retrying member find (attempt ${attempt}, delay ${ms(
            nextDelayMs
          )})`,
          error
        )
      },
    }
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
    let requestObject: apiPayload.MemberCreateRequest
    try {
      requestObject = apiPayload.checkMemberCreateRequest(
        await request.json()
      )
    } catch (error: unknown) {
      if (error instanceof payload.PayloadError) {
        return {
          status: 400,
          headers: {
            'content-type': 'application/json',
          },
          body: {
            error: error.message,
          },
        }
      } else {
        throw error
      }
    }
    const id = crypto.hash(
      Buffer.from(requestObject.requestId, 'hex')
    )
    try {
      await errorModule.retry(
        ctx,
        () =>
          memberQuery.create(
            ctx.databaseApiWrite,
            id,
            requestObject.email,
            requestObject.handle
          ),
        {
          maxAttempts: 15,
          minDelayMs: time.interval({ milliseconds: 10 }),
          windowMs: time.interval({ seconds: 30 }),
          onError(error, attempt, nextDelayMs) {
            if (
              query.isQueryError(error) &&
              error.code === query.errorCode.uniqueViolation
            ) {
              const constraintName = query.getUniqueViolationConstraintName(
                error
              )
              if (constraintName === 'member_email_key') {
                throw new apiPayload.MemberCreateConflictError(
                  'email'
                )
              } else if (
                constraintName === 'member_handle_key'
              ) {
                throw new apiPayload.MemberCreateConflictError(
                  'handle'
                )
              }
            }
            console.error(
              `Retrying member create (attempt ${attempt}, delay ${ms(
                nextDelayMs
              )})`,
              error
            )
          },
        }
      )
    } catch (error: unknown) {
      if (
        error instanceof
        apiPayload.MemberCreateConflictError
      ) {
        const conflictResponseObject: apiPayload.MemberCreateConflictResponse = {
          conflict: error.field,
        }
        return {
          status: 409,
          headers: {
            'content-type': 'application/json',
          },
          body: apiPayload.checkMemberCreateConflictResponse(
            conflictResponseObject
          ),
        }
      }
      throw error
    }
    const responseObject: apiPayload.MemberCreateResponse = {
      id: id.toString('hex'),
    }
    return {
      status: 200,
      headers: {
        'content-type': 'application/json',
      },
      body: apiPayload.checkMemberCreateResponse(
        responseObject
      ),
    }
  }
)

export const routes = [apiMessage, apiMemberCreate]

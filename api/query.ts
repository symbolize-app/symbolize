import * as message from '@fe/core/message.ts'
import type * as db from '@fe/db/index.ts'
import * as memberQuery from '@fe/db/query/member.ts'
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

class MemberCreateUniqueConstraintError extends Error {
  field: 'email' | 'handle'
  constructor(cause: Error, field: 'email' | 'handle') {
    super(cause.message)
    this.field = field
  }
}

export const apiMemberCreate = route.define<
  errorModule.Context & db.WriteContext
>(
  ['POST'],
  /^\/api\/member\/create$/,
  async (ctx, request) => {
    const requestJson = await request.json()
    const validatedRequestObject = {
      id: crypto.hash(
        Buffer.from(requestJson.requestId as string, 'hex')
      ),
      email: requestJson.email as string,
      handle: requestJson.handle as string,
    }
    try {
      await errorModule.retry(
        ctx,
        () =>
          memberQuery.create(
            ctx.databaseApiWrite,
            validatedRequestObject.id,
            validatedRequestObject.email,
            validatedRequestObject.handle
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
                throw new MemberCreateUniqueConstraintError(
                  error,
                  'email'
                )
              } else if (
                constraintName === 'member_handle_key'
              ) {
                throw new MemberCreateUniqueConstraintError(
                  error,
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
        error instanceof MemberCreateUniqueConstraintError
      ) {
        return {
          status: 409,
          headers: {
            'content-type': 'application/json',
          },
          body: {
            conflict: error.field,
          },
        }
      }
      throw error
    }
    return {
      status: 200,
      headers: {
        'content-type': 'application/json',
      },
      body: {
        id: validatedRequestObject.id.toString('hex'),
      },
    }
  }
)

export const routes = [apiMessage, apiMemberCreate]

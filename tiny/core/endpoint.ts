import * as payload from '@tiny/core/payload.ts'
import type * as typeFest from 'type-fest'

export type BaseEndpoint<Method extends string> = {
  method: Method
  path: string
}

function defineBaseEndpoint<Method extends string>(
  method: Method,
  path: string
): BaseEndpoint<Method> {
  return {
    method,
    path,
  }
}

export type Request<
  Endpoint extends BaseEndpoint<string>
> = Endpoint extends {
  checkRequest: payload.Validator<infer Request>
}
  ? Request
  : never

export type OkResponse<
  Endpoint extends BaseEndpoint<string>
> = Endpoint extends {
  checkOkResponse: payload.Validator<infer OkResponse>
}
  ? OkResponse
  : never

export type ConflictResponse<
  Endpoint extends BaseEndpoint<string>
> = Endpoint extends {
  checkConflictResponse: payload.Validator<
    infer ConflictResponse
  >
}
  ? ConflictResponse extends { conflict: string }
    ? ConflictResponse
    : never
  : never

export type ConflictError<
  Endpoint extends BaseEndpoint<string>
> = payload.ConflictError<ConflictResponse<Endpoint>>

export type GetEndpoint<
  Request extends Record<string, string>,
  OkResponse extends typeFest.JsonObject
> = BaseEndpoint<'GET'> & {
  checkRequest: payload.Validator<Request>
  checkOkResponse: payload.Validator<OkResponse>
}

export function defineGetEndpoint<
  Request extends Record<string, string>,
  OkResponse extends typeFest.JsonObject
>(
  path: string,
  checkRequest: payload.Validator<Request>,
  checkOkResponse: payload.Validator<OkResponse>
): GetEndpoint<Request, OkResponse> {
  return {
    ...defineBaseEndpoint('GET', path),
    checkRequest,
    checkOkResponse,
  }
}

export type PostEndpoint<
  Request extends typeFest.JsonObject,
  OkResponse extends typeFest.JsonObject
> = BaseEndpoint<'POST'> & {
  checkRequest: payload.Validator<Request>
  checkOkResponse: payload.Validator<OkResponse>
}

export function definePostEndpoint<
  Request extends typeFest.JsonObject,
  OkResponse extends typeFest.JsonObject
>(
  path: string,
  checkRequest: payload.Validator<Request>,
  checkOkResponse: payload.Validator<OkResponse>
): PostEndpoint<Request, OkResponse> {
  return {
    ...defineBaseEndpoint('POST', path),
    checkRequest,
    checkOkResponse,
  }
}

export type ConflictPostEndpoint<
  Request extends typeFest.JsonObject,
  OkResponse extends typeFest.JsonObject,
  ConflictResponse extends typeFest.JsonObject & {
    conflict: string
  }
> = PostEndpoint<Request, OkResponse> & {
  conflictError: new (
    field: ConflictResponse['conflict']
  ) => payload.ConflictError<ConflictResponse>
  checkConflictResponse: payload.Validator<ConflictResponse>
}

export function defineConflictPostEndpoint<
  Request extends typeFest.JsonObject,
  OkResponse extends typeFest.JsonObject,
  ConflictResponse extends typeFest.JsonObject & {
    conflict: string
  }
>(
  path: string,
  checkRequest: payload.Validator<Request>,
  checkOkResponse: payload.Validator<OkResponse>,
  checkConflictResponse: payload.Validator<ConflictResponse>
): ConflictPostEndpoint<
  Request,
  OkResponse,
  ConflictResponse
> {
  class ConflictError extends payload.ConflictError<ConflictResponse> {}
  return {
    ...definePostEndpoint(
      path,
      checkRequest,
      checkOkResponse
    ),
    conflictError: ConflictError,
    checkConflictResponse,
  }
}

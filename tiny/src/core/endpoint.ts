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

export type RequestData<
  Endpoint extends BaseEndpoint<string>
> = Endpoint extends {
  checkRequest: payload.Validator<infer RequestData>
}
  ? RequestData
  : never

export type OkResponseData<
  Endpoint extends BaseEndpoint<string>
> = Endpoint extends {
  checkOkResponse: payload.Validator<infer OkResponseData>
}
  ? OkResponseData
  : never

export type ConflictResponseData<
  Endpoint extends BaseEndpoint<string>
> = Endpoint extends {
  checkConflictResponse: payload.Validator<
    infer ConflictResponseData
  >
}
  ? ConflictResponseData extends { conflict: string }
    ? ConflictResponseData
    : never
  : never

export type ConflictError<
  Endpoint extends BaseEndpoint<string>
> = payload.ConflictError<ConflictResponseData<Endpoint>>

export type GetEndpoint<
  RequestData extends Record<string, string>,
  OkResponseData extends typeFest.JsonObject
> = BaseEndpoint<'GET'> & {
  checkRequest: payload.Validator<RequestData>
  checkOkResponse: payload.Validator<OkResponseData>
}

export function defineGetEndpoint<
  RequestData extends Record<string, string>,
  OkResponseData extends typeFest.JsonObject
>(
  path: string,
  checkRequest: payload.Validator<RequestData>,
  checkOkResponse: payload.Validator<OkResponseData>
): GetEndpoint<RequestData, OkResponseData> {
  return {
    ...defineBaseEndpoint('GET', path),
    checkRequest,
    checkOkResponse,
  }
}

export type PostEndpoint<
  RequestData extends typeFest.JsonObject,
  OkResponseData extends typeFest.JsonObject
> = BaseEndpoint<'POST'> & {
  checkRequest: payload.Validator<RequestData>
  checkOkResponse: payload.Validator<OkResponseData>
}

export function definePostEndpoint<
  RequestData extends typeFest.JsonObject,
  OkResponseData extends typeFest.JsonObject
>(
  path: string,
  checkRequest: payload.Validator<RequestData>,
  checkOkResponse: payload.Validator<OkResponseData>
): PostEndpoint<RequestData, OkResponseData> {
  return {
    ...defineBaseEndpoint('POST', path),
    checkRequest,
    checkOkResponse,
  }
}

export type ConflictPostEndpoint<
  RequestData extends typeFest.JsonObject,
  OkResponseData extends typeFest.JsonObject,
  ConflictResponseData extends typeFest.JsonObject & {
    conflict: string
  }
> = PostEndpoint<RequestData, OkResponseData> & {
  conflictError: new (
    field: ConflictResponseData['conflict']
  ) => payload.ConflictError<ConflictResponseData>
  checkConflictResponse: payload.Validator<ConflictResponseData>
}

export function defineConflictPostEndpoint<
  RequestData extends typeFest.JsonObject,
  OkResponseData extends typeFest.JsonObject,
  ConflictResponseData extends typeFest.JsonObject & {
    conflict: string
  }
>(
  path: string,
  checkRequest: payload.Validator<RequestData>,
  checkOkResponse: payload.Validator<OkResponseData>,
  checkConflictResponse: payload.Validator<ConflictResponseData>
): ConflictPostEndpoint<
  RequestData,
  OkResponseData,
  ConflictResponseData
> {
  class ConflictError extends payload.ConflictError<ConflictResponseData> {}
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

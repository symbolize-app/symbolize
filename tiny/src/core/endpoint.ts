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
  Endpoint extends {
    checkRequest: payload.Validator<typeFest.JsonObject>
  }
> = ReturnType<Endpoint['checkRequest']>

export type OkResponseData<
  Endpoint extends {
    checkOkResponse: payload.Validator<typeFest.JsonObject>
  }
> = ReturnType<Endpoint['checkOkResponse']>

export type ConflictResponseData<
  Endpoint extends {
    checkConflictResponse: payload.Validator<
      typeFest.JsonObject & { conflict: string }
    >
  }
> = ReturnType<Endpoint['checkConflictResponse']>

export type ConflictError<
  Endpoint extends {
    checkConflictResponse: payload.Validator<
      typeFest.JsonObject & { conflict: string }
    >
  }
> = payload.ConflictError<ConflictResponseData<Endpoint>>

export type GetEndpoint<
  RequestData extends Record<string, string> = Record<
    string,
    string
  >,
  OkResponseData extends typeFest.JsonObject = typeFest.JsonObject
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
  RequestData extends typeFest.JsonObject = typeFest.JsonObject,
  OkResponseData extends typeFest.JsonObject = typeFest.JsonObject,
  ConflictResponseData extends typeFest.JsonObject & {
    conflict: string
  } = typeFest.JsonObject & {
    conflict: string
  }
> = PostEndpoint<RequestData, OkResponseData> & {
  conflictError: new (
    field: string
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

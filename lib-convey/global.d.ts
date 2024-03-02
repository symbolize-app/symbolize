declare module 'core-js-pure/actual/array/from-async' {
  function fromAsync<T>(
    iterableOrArrayLike:
      | ArrayLike<Promise<T> | T>
      | AsyncIterable<T>
      | Iterable<Promise<T> | T>,
  ): Promise<T[]>

  function fromAsync<T, U>(
    iterableOrArrayLike: ArrayLike<T> | AsyncIterable<T> | Iterable<T>,
    mapFn: (value: Awaited<T>) => U,
    thisArg?: unknown,
  ): Promise<Awaited<U>[]>

  export = fromAsync
}

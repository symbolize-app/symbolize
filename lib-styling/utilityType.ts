export type ReadonlyParameters<T extends (...args: never) => unknown> =
  Readonly<
    T extends (...args: infer P) => unknown ? P
    : T extends (...args: readonly [...infer P]) => unknown ? P
    : never
  >

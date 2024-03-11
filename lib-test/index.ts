import * as testIsDeepEqual from '@/isDeepEqual.ts'

export type Test<CustomContext = unknown> = (
  ctx: CustomContext,
) => Promise<void> | void

export interface TestModule<CustomContext = unknown> {
  readonly tests: Readonly<Record<string, Test<CustomContext>>>
  readonly url: string
}

export interface TestCollectionModule<CustomContext = unknown> {
  readonly all: TestCollection<CustomContext>
}

export type TestCollection<CustomContext = unknown> =
  () => readonly Promise<
    TestCollectionModule<CustomContext> | TestModule<CustomContext>
  >[]

export enum AssertionMode {
  error = 'error',
  diff = 'diff',
}

export class AssertionError extends Error {
  constructor(
    message: string,
    readonly actual: unknown,
    readonly expected: unknown,
    readonly mode: AssertionMode = AssertionMode.error,
  ) {
    super(message)
  }
}

export function mock<Func extends (...args: never) => unknown>(
  returnValues: readonly Func[],
): Func {
  return mockWithHistory(returnValues)[0]
}

export function mockWithHistory<Func extends (...args: never) => unknown>(
  returnValues: readonly Func[],
): readonly [Func, readonly Parameters<Func>[]] {
  let i = 0
  const mutableHistory: Parameters<Func>[] = []
  const callback = ((...args) => {
    if (i === returnValues.length) {
      throw new Error('called too many times')
    } else {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
      const result = returnValues[i]!(...args)
      mutableHistory.push(args)
      i += 1
      return result
    }
  }) as Func
  return [callback, mutableHistory]
}

export function repeatMock<Func extends (...args: never) => unknown>(
  repeat: number,
  returnValue: Func,
): Func {
  return repeatMockWithHistory(repeat, returnValue)[0]
}

export function repeatMockWithHistory<
  Func extends (...args: never) => unknown,
>(
  repeat: number,
  returnValue: Func,
): readonly [Func, readonly Parameters<Func>[]] {
  const mutableReturnValues: Func[] = []
  for (let i = 0; i < repeat; i++) {
    mutableReturnValues.push(returnValue)
  }
  return mockWithHistory(mutableReturnValues)
}

export interface SyncPromise<Value> {
  readonly isSettled: boolean
  readonly rejectedValue: unknown
  readonly resolvedValue: Value
}

class SyncPromiseImpl<Value> implements SyncPromise<Value> {
  private mutableIsRejected: boolean = false
  private mutableIsResolved: boolean = false
  private mutableRejectedValue: unknown = undefined
  private mutableResolvedValue: Value | undefined = undefined

  private constructor() {}

  get isSettled(): boolean {
    return this.mutableIsRejected || this.mutableIsRejected
  }

  get rejectedValue(): unknown {
    if (this.mutableIsResolved) {
      throw new Error('Promise resolved')
    } else if (!this.mutableIsRejected) {
      throw new Error('Promise not rejected yet')
    } else {
      return this.mutableRejectedValue
    }
  }

  get resolvedValue(): Value {
    if (this.mutableIsRejected) {
      throw this.mutableRejectedValue
    } else if (!this.mutableIsResolved) {
      throw new Error('Promise not resolved yet')
    } else {
      return this.mutableResolvedValue as Value
    }
  }

  static build<Value>(
    promise: Promise<Value> | Value,
  ): Readonly<SyncPromiseImpl<Value>> {
    const syncPromise = new SyncPromiseImpl<Value>()
    Promise.resolve(promise).then(
      (value) => {
        syncPromise.mutableIsResolved = true
        syncPromise.mutableResolvedValue = value
      },
      (value) => {
        syncPromise.mutableIsRejected = true
        syncPromise.mutableRejectedValue = value
      },
    )
    return syncPromise
  }
}

export function sync<Value>(
  promise: Promise<Value> | Value,
): SyncPromise<Value> {
  return SyncPromiseImpl.build(promise)
}

export function assert(actual: unknown): asserts actual {
  if (!actual) {
    throw new AssertionError('Not truthy', actual, '(truthy)')
  }
}

export function assertEquals<Value>(actual: Value, expected: Value): void {
  if (actual !== expected) {
    throw new AssertionError('Not equal', actual, expected)
  }
}

export function assertInstanceOf<Result, Args extends unknown[]>(
  actual: unknown,
  expectedType: abstract new (...args: Args) => Result,
): asserts actual is Result {
  if (!(actual instanceof expectedType)) {
    throw new AssertionError(
      'Not instance of',
      (typeof actual === 'object' ?
        actual?.constructor.name
      : undefined) ?? typeof actual,
      expectedType.name,
    )
  }
}

export function assertDeepEquals<Value>(
  actual: Value,
  expected: Value,
): void {
  if (!testIsDeepEqual.isDeepEqual(actual, expected)) {
    throw new AssertionError(
      'Not deep equal',
      actual,
      expected,
      AssertionMode.diff,
    )
  }
}

export function assertThrows(callback: () => unknown): unknown {
  let result: unknown
  try {
    result = callback()
  } catch (error) {
    return error
  }
  throw new AssertionError('No error thrown', result, '<error>')
}

export async function assertThrowsAsync(
  callback: () => Promise<unknown>,
): Promise<unknown> {
  let result: unknown
  try {
    result = await callback()
  } catch (error) {
    return error
  }
  throw new AssertionError('No error thrown', result, '<error>')
}

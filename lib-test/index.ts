/* eslint-disable no-console */

import * as testIsDeepEqual from '@/isDeepEqual.ts'
import * as testRandom from '@/random.ts'
import * as testTime from '@/time.ts'
import type * as random from '@intertwine/lib-random'
import type * as time from '@intertwine/lib-time'
import * as diff from 'diff'
import ms from 'ms'

export type Test<CustomContext = unknown> = (
  ctx: Context & CustomContext,
) => Promise<void> | void

export type Context = random.Context & testTime.Context

export type RunContext = time.Context

interface TestModule<CustomContext = unknown> {
  readonly tests: Readonly<Record<string, Test<CustomContext>>>
  readonly url: string
}

export type TestCollection<CustomContext = unknown> =
  () => readonly Promise<TestModule<CustomContext>>[]

interface TestCollectionModule<CustomContext = unknown> {
  readonly all: TestCollection<CustomContext>
}

enum AssertionMode {
  error = 'error',
  diff = 'diff',
}

class AssertionError extends Error {
  constructor(
    message: string,
    readonly actual: unknown,
    readonly expected: unknown,
    readonly mode: AssertionMode = AssertionMode.error,
  ) {
    super(message)
  }
}

export async function runAll<CustomContext = unknown>(
  ctx: CustomContext & RunContext,
  testCollectionModules: readonly Promise<
    TestCollectionModule<CustomContext>
  >[],
): Promise<boolean> {
  const testModules = ([] as Promise<TestModule<CustomContext>>[]).concat(
    ...(await Promise.all(testCollectionModules)).map(
      (testCollectionModule) => testCollectionModule.all(),
    ),
  )
  const resolvedTestModules = await Promise.all(testModules)
  const start = ctx.time.performanceNow()
  let pass = 0
  let fail = 0
  let onlyMode = false
  for (const testModule of resolvedTestModules) {
    const { tests } = testModule
    for (const [testName] of Object.entries(tests)) {
      if (testName.startsWith('O:')) {
        onlyMode = true
      }
    }
  }
  console.group('Testing...')
  for (const testModule of resolvedTestModules) {
    const { url, tests } = testModule
    let testUrlPrinted = false
    for (const [testName, test] of Object.entries(tests)) {
      if (onlyMode && !testName.startsWith('O:')) {
        continue
      }
      try {
        const testContext: Context & CustomContext = {
          ...ctx,
          random: testRandom.RandomImpl.build(),
          time: testTime.TimeImpl.build(),
        }
        await test(testContext)
        pass += 1
      } catch (error) {
        const message =
          error instanceof Error ?
            error.message
          : `Error value ${JSON.stringify(error)}`
        let stack = error instanceof Error ? error.stack : undefined

        const assertionInfo =
          (
            typeof error === 'object' &&
            error !== null &&
            'actual' in error &&
            'expected' in error &&
            'mode' in error
          ) ?
            {
              actual: (error as { actual: unknown }).actual,
              expected: (error as { expected: unknown }).expected,
              mode: (error as { mode: unknown }).mode,
            }
          : undefined

        if (!testUrlPrinted) {
          if (fail === 0) {
            console.group(url)
          } else {
            console.groupCollapsed(url)
          }
          testUrlPrinted = true
        }
        if (fail === 0) {
          console.group(testName)
        } else {
          console.groupCollapsed(testName)
        }
        console.log(`%c${message}`, 'color: crimson')
        console.groupCollapsed('Details')
        if (assertionInfo) {
          console.log('%cExpected', 'color: green', assertionInfo.expected)
          console.log('%cActual', 'color: crimson', assertionInfo.actual)
          if (assertionInfo.mode === AssertionMode.diff) {
            const diffSections = diff.diffJson(
              assertionInfo.expected as object | string,
              assertionInfo.actual as object | string,
            )
            const mutableOutputLines = []
            for (const diffSection of diffSections) {
              let prefix: string
              if (diffSection.added) {
                prefix = '+'
              } else if (diffSection.removed) {
                prefix = '-'
              } else {
                prefix = ' '
              }
              for (const diffLine of diffSection.value.split('\n')) {
                if (diffLine) {
                  mutableOutputLines.push(`${prefix} ${diffLine}`)
                }
              }
            }
            console.log(mutableOutputLines.join('\n'))
          }
          stack = stack?.replace(/^Assertion/, '')
        }
        if (stack) {
          console.log(stack, 'color: grey')
        }
        console.groupEnd()
        console.groupEnd()
        fail += 1
      }
    }
    if (testUrlPrinted) {
      console.groupEnd()
    }
  }

  const end = ctx.time.performanceNow()
  const elapsed = ms(Math.round(end - start))

  console.log(`%cPass: ${pass}`, 'font-weight: bold; color: green')
  console.log(
    `%cFail: ${fail}`,
    `font-weight: bold; color: ${fail ? 'crimson' : 'green'}`,
  )
  console.log(`Elapsed: ${elapsed}`)
  console.groupEnd()
  return fail === 0
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
    throw new AssertionError('Not true', actual, '(truthy)')
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

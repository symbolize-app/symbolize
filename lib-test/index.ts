/* eslint-disable no-console */

import * as testIsDeepEqual from '@/isDeepEqual.ts'
import * as testRandom from '@/random.ts'
import * as testTime from '@/time.ts'
import type * as random from '@intertwine/lib-random'
import type * as time from '@intertwine/lib-time'
import * as diff from 'diff'
import ms from 'ms'

export type Test<CustomContext = unknown> = (
  ctx: Context & CustomContext
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

class AssertionError extends Error {
  readonly actual: unknown
  readonly diff: boolean
  readonly expected: unknown

  constructor(
    message: string,
    actual: unknown,
    expected: unknown,
    enableDiff?: 'diff' | undefined
  ) {
    super(message)
    this.actual = actual
    this.expected = expected
    this.diff = !!enableDiff
  }
}

export async function runAll<CustomContext = unknown>(
  ctx: CustomContext & RunContext,
  testCollectionModules: readonly Promise<
    TestCollectionModule<CustomContext>
  >[]
): Promise<boolean> {
  const testModules = ([] as Promise<TestModule<CustomContext>>[]).concat(
    ...(await Promise.all(testCollectionModules)).map(
      (testCollectionModule) => testCollectionModule.all()
    )
  )
  const resolvedTestModules = await Promise.all(testModules)
  const start = ctx.time.performanceNow()
  let pass = 0
  let fail = 0
  console.group('Testing...')
  for (const testModule of resolvedTestModules) {
    const { url, tests } = testModule
    let testUrlPrinted = false
    for (const [testName, test] of Object.entries(tests)) {
      try {
        const testContext: Context & CustomContext = {
          ...ctx,
          ...testTime.initContext(),
          ...testRandom.initContext(),
        }
        await test(testContext)
        pass += 1
      } catch (error) {
        const message =
          error instanceof Error
            ? error.message
            : `Error value ${JSON.stringify(error)}`
        let stack = error instanceof Error ? error.stack : undefined

        const assertionInfo =
          typeof error === 'object' &&
          error !== null &&
          'actual' in error &&
          'expected' in error &&
          'diff' in error
            ? {
                actual: (error as { actual: unknown }).actual,
                diff: (error as { diff: unknown }).diff,
                expected: (error as { expected: unknown }).expected,
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
          if (assertionInfo.diff) {
            const diffSections = diff.diffJson(
              assertionInfo.expected as object | string,
              assertionInfo.actual as object | string
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
    `font-weight: bold; color: ${fail ? 'crimson' : 'green'}`
  )
  console.log(`Elapsed: ${elapsed}`)
  console.groupEnd()
  return fail === 0
}

export const mockHistory = Symbol('mockHistory')

export function mock<
  Mock extends (...args: Readonly<unknown[]>) => unknown,
>(
  returnValues: readonly (() => ReturnType<Mock>)[]
): Mock & { readonly [mockHistory]: readonly Parameters<Mock>[] } {
  let i = 0
  const mutableCallback = ((...args: Parameters<Mock>) => {
    if (i === returnValues.length) {
      throw new Error('called too many times')
    } else {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
      const result = returnValues[i]!()
      mutableCallback[mockHistory].push(args)
      i += 1
      return result
    }
  }) as Mock & { [mockHistory]: Parameters<Mock>[] }
  mutableCallback[mockHistory] = []
  return mutableCallback
}

export interface SyncPromise<Value> {
  readonly isSettled: boolean
  readonly rejectedValue: unknown
  readonly resolvedValue: Value
}

export function sync<Value>(
  promise: Promise<Value> | Value
): SyncPromise<Value> {
  let isResolved = false
  let resolvedValue: Value | undefined = undefined
  let isRejected = false
  let rejectedValue: unknown = undefined
  const result = {
    get isSettled() {
      return isResolved || isRejected
    },
    get rejectedValue() {
      if (isResolved) {
        throw new AssertionError('Promise resolved', result, '<rejected>')
      } else if (!isRejected) {
        throw new Error('Promise not rejected yet')
      } else {
        return rejectedValue
      }
    },
    get resolvedValue() {
      if (isRejected) {
        throw rejectedValue
      } else if (!isResolved) {
        throw new Error('Promise not resolved yet')
      } else {
        return resolvedValue as Value
      }
    },
  }
  Promise.resolve(promise).then(
    (value) => {
      isResolved = true
      resolvedValue = value
    },
    (value) => {
      isRejected = true
      rejectedValue = value
    }
  )
  return result
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
  expectedType: abstract new (...args: Args) => Result
): asserts actual is Result {
  if (!(actual instanceof expectedType)) {
    throw new AssertionError(
      'Not instance of',
      (typeof actual === 'object'
        ? actual?.constructor.name
        : undefined) ?? typeof actual,
      expectedType.name
    )
  }
}

export function assertDeepEquals<Value>(
  actual: Value,
  expected: Value
): void {
  if (!testIsDeepEqual.isDeepEqual(actual, expected)) {
    throw new AssertionError('Not deep equal', actual, expected, 'diff')
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

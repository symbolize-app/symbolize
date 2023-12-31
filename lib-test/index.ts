import * as testIsDeepEqual from '@/isDeepEqual.ts'
import * as testRandom from '@/random.ts'
import * as testTime from '@/time.ts'
import type * as random from '@intertwine/lib-random'
import type * as time from '@intertwine/lib-time'
import * as diff from 'diff'
import ms from 'ms'

export type Test<CustomContext = unknown> = (
  ctx: CustomContext & Context
) => void | Promise<void>

export type Context = testTime.Context & random.Context

export type RunContext = time.Context

type TestModule<CustomContext = unknown> = {
  url: string
  tests: {
    [testName: string]: Test<CustomContext>
  }
}

export type TestCollection<CustomContext = unknown> = () => Promise<
  TestModule<CustomContext>
>[]

type TestCollectionModule<CustomContext = unknown> = {
  all: TestCollection<CustomContext>
}

class AssertionError extends Error {
  actual: unknown
  expected: unknown
  diff: boolean

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

export async function runAll<
  CustomContext extends Record<string, unknown> = Record<string, unknown>,
>(
  ctx: CustomContext & RunContext,
  testCollectionModules: Promise<TestCollectionModule<CustomContext>>[]
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
        const testContext: CustomContext & Context = {
          ...ctx,
          ...testTime.initContext(),
          ...testRandom.initContext(),
        }
        await test(testContext)
        pass += 1
      } catch (error) {
        const basicInfo =
          error instanceof Error
            ? {
                message: error.message,
                stack: error.stack,
              }
            : {
                message: `Error value ${JSON.stringify(error)}`,
                stack: undefined,
              }

        const assertionInfo =
          typeof error == 'object' &&
          error !== null &&
          'actual' in error &&
          'expected' in error &&
          'diff' in error
            ? {
                actual: (error as { actual: unknown }).actual,
                expected: (error as { expected: unknown }).expected,
                diff: (error as { diff: unknown }).diff,
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
        console.log(`%c${basicInfo.message}`, 'color: crimson')
        console.groupCollapsed('Details')
        if (assertionInfo) {
          console.log('%cExpected', 'color: green', assertionInfo.expected)
          console.log('%cActual', 'color: crimson', assertionInfo.actual)
          if (assertionInfo.diff) {
            const diffSections = diff.diffJson(
              assertionInfo.expected as string | object,
              assertionInfo.actual as string | object
            )
            const outputLines = []
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
                  outputLines.push(`${prefix} ${diffLine}`)
                }
              }
            }
            console.log(outputLines.join('\n'))
          }
          basicInfo.stack = basicInfo.stack?.replace(/^Assertion/, '')
        }
        if (basicInfo.stack) {
          console.log(basicInfo.stack, 'color: grey')
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
  return fail == 0
}

export const mockHistory = Symbol('mockHistory')

export function mock<
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  Mock extends (...args: any[]) => unknown,
>(
  returnValues: (() => ReturnType<Mock>)[]
): Mock & { [mockHistory]: Parameters<Mock>[] } {
  let i = 0
  const callback = ((...args: Parameters<Mock>) => {
    if (i === returnValues.length) {
      throw new Error('called too many times')
    } else {
      const result = returnValues[i]!()
      callback[mockHistory].push(args)
      i += 1
      return result
    }
  }) as Mock & { [mockHistory]: Parameters<Mock>[] }
  callback[mockHistory] = []
  return callback
}

export type SyncPromise<Value> = {
  isSettled: boolean
  resolvedValue: Value
  rejectedValue: unknown
}

export function sync<Value>(
  promise: Value | Promise<Value>
): SyncPromise<Value> {
  let isResolved = false
  let resolvedValue: Value | undefined = undefined
  let isRejected = false
  let rejectedValue: unknown = undefined
  const result = {
    get isSettled() {
      return isResolved || isRejected
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
    get rejectedValue() {
      if (isResolved) {
        throw new AssertionError('Promise resolved', result, '<rejected>')
      } else if (!isRejected) {
        throw new Error('Promise not rejected yet')
      } else {
        return rejectedValue
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

export function assertInstanceOf<
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  Constructor extends new (...args: any) => any,
>(
  actual: unknown,
  expectedType: Constructor
): asserts actual is InstanceType<Constructor> {
  if (!(actual instanceof expectedType)) {
    throw new AssertionError(
      'Not instance of',
      (typeof actual === 'object'
        ? actual?.constructor?.name
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

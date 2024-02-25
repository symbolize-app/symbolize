/* eslint-disable no-console */

import * as test from '@intertwine/lib-test'
import type * as time from '@intertwine/lib-time'
import * as diff from 'diff'
import ms from 'ms'

export type RunContext = time.Context

interface TestCollectionModule<CustomContext = unknown> {
  readonly all: test.TestCollection<CustomContext>
}

export async function runAll<CustomContext = unknown>(
  ctx: time.Context,
  testCollectionModules: readonly Promise<
    TestCollectionModule<CustomContext>
  >[],
  buildTestContext: (
    defer: (callback: () => Promise<void> | void) => void,
  ) => CustomContext,
): Promise<boolean> {
  const testModules = (
    [] as Promise<test.TestModule<CustomContext>>[]
  ).concat(
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
    for (const [testName, test_] of Object.entries(tests)) {
      if (onlyMode && !testName.startsWith('O:')) {
        continue
      }
      try {
        const mutableDeferred: (() => Promise<void> | void)[] = []
        await test_(
          buildTestContext((callback) =>
            mutableDeferred.unshift(callback),
          ),
        )
        for (const callback of mutableDeferred) {
          await callback()
        }
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
          if (assertionInfo.mode === test.AssertionMode.diff) {
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

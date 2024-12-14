/* eslint-disable no-console */

import * as test from '@symbolize/lib-test'
import type * as time from '@symbolize/lib-time'
import arrayFromAsync from 'core-js-pure/actual/array/from-async'
import * as diff from 'diff'
import ms from 'ms'

export type RunContext = time.Context

type ContextBuilder<CustomContext> = {
  [Key in keyof CustomContext]: (
    defer: (callback: () => Promise<void> | void) => void,
  ) => CustomContext[Key]
}

export async function runAll<CustomContext = unknown>(
  ctx: time.Context,
  testCollectionModulePromises: readonly Promise<
    test.TestCollectionModule<CustomContext>
  >[],
  contextBuilder: ContextBuilder<CustomContext>,
): Promise<boolean> {
  const testModules = await arrayFromAsync(
    resolveAllTestModules(testCollectionModulePromises),
  )
  const start = ctx.time.performanceNow()
  let pass = 0
  let fail = 0
  const onlyMode = isOnlyMode(testModules)
  console.group('Testing...')
  for (const testModule of testModules) {
    const [modulePass, moduleFail] = await runModule(
      testModule,
      onlyMode,
      contextBuilder,
    )
    pass += modulePass
    fail += moduleFail
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

async function* resolveAllTestModules<CustomContext>(
  testCollectionModulePromises: readonly Promise<
    test.TestCollectionModule<CustomContext>
  >[],
): AsyncIterableIterator<test.TestModule<CustomContext>> {
  for (const testCollectionModule of await Promise.all(
    testCollectionModulePromises,
  )) {
    for await (const result of resolveTestCollectionModules(
      testCollectionModule,
    )) {
      yield result
    }
  }
}

async function* resolveTestCollectionModules<CustomContext>(
  testCollectionModule: test.TestCollectionModule<CustomContext>,
): AsyncIterableIterator<test.TestModule<CustomContext>> {
  for (const item of await Promise.all(testCollectionModule.all())) {
    if ('all' in item) {
      for await (const result of resolveTestCollectionModules(item)) {
        yield result
      }
    } else {
      yield item
    }
  }
}

function isOnlyMode<CustomContext>(
  testModules: readonly test.TestModule<CustomContext>[],
): boolean {
  for (const testModule of testModules) {
    const { tests } = testModule
    for (const [testName] of Object.entries(tests)) {
      if (testName.startsWith('O:')) {
        return true
      }
    }
  }
  return false
}

async function runModule<CustomContext>(
  testModule: test.TestModule<CustomContext>,
  onlyMode: boolean,
  contextBuilder: ContextBuilder<CustomContext>,
): Promise<[pass: number, fail: number]> {
  let pass = 0
  let fail = 0
  const { url, tests } = testModule
  let testUrlPrinted = false
  for (const [testName, test_] of Object.entries(tests)) {
    if (onlyMode && !testName.startsWith('O:')) {
      continue
    }
    try {
      const mutableDeferred: (() => Promise<void> | void)[] = []
      const mutableCtx = {} as Partial<CustomContext>
      const ctx = new Proxy(contextBuilder, {
        get(_, key) {
          const ctxKey = key as keyof CustomContext
          let value = mutableCtx[ctxKey]
          if (!value) {
            value = contextBuilder[ctxKey]((callback) =>
              mutableDeferred.unshift(callback),
            )
            mutableCtx[ctxKey] = value
          }
          return value
        },
      }) as CustomContext
      await test_(ctx)
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
  return [pass, fail]
}

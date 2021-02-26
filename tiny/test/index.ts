import * as chai from 'chai'
import chaiDom from 'chai-dom'
import prettyMs from 'pretty-ms'

type Test = () => void | Promise<void>

type TestModule = {
  url: string
  tests: {
    [testName: string]: Test
  }
}

export async function runAll(
  ...allTestModules: Promise<TestModule>[]
): Promise<void> {
  console.group('Testing...')
  chai.use(chaiDom)
  const start = window.performance.now()
  let pass = 0
  let fail = 0
  for (const testModule of await Promise.all(
    allTestModules
  )) {
    const { url, tests } = testModule
    let testUrlPrinted = false
    for (const testName in tests) {
      const test = tests[testName]
      try {
        await test()
        pass += 1
      } catch (error: unknown) {
        const info =
          error instanceof Error
            ? {
                message: error.message,
                stack: error.stack,
              }
            : {
                message: `Error value ${JSON.stringify(
                  error
                )}`,
                stack: undefined,
              }

        const assertion =
          typeof error == 'object' &&
          error !== null &&
          'actual' in error &&
          'expected' in error
            ? {
                actual: (error as { actual: unknown })
                  .actual,
                expected: (error as { expected: unknown })
                  .expected,
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
        console.log(`%c${info.message}`, 'color: crimson')
        console.groupCollapsed('Details')
        if (assertion) {
          console.log(
            '%cActual',
            'color: crimson',
            assertion.actual
          )
          console.log(
            '%cExpected',
            'color: green',
            assertion.expected
          )
          info.stack = info.stack?.replace(/^Assertion/, '')
        }
        if (info.stack) {
          console.log(info.stack, 'color: grey')
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
  const end = window.performance.now()
  const elapsed = prettyMs(end - start)
  console.log(
    `%cPass: ${pass}`,
    'font-weight: bold; color: green'
  )
  console.log(
    `%cFail: ${fail}`,
    `font-weight: bold; color: ${
      fail ? 'crimson' : 'green'
    }`
  )
  console.log(`Elapsed: ${elapsed}`)
  console.groupEnd()
}

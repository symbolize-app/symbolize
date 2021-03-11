import chai from 'chai'
import chaiDom from 'chai-dom'
import ms from 'ms'

export type Test = () => void | Promise<void>

type TestModule = {
  url: string
  tests: {
    [testName: string]: Test
  }
}

export type TestCollection = () => Promise<TestModule>[]

type TestCollectionModule = {
  all: TestCollection
}

export async function runAll(
  ctx: { now: () => number },
  testCollectionModules: TestCollectionModule[]
): Promise<boolean> {
  const testModules = ([] as Promise<TestModule>[]).concat(
    ...testCollectionModules.map((testCollectionModule) =>
      testCollectionModule.all()
    )
  )
  const resolvedTestModules = await Promise.all(testModules)
  chai.use(chaiDom)
  const start = ctx.now()
  let pass = 0
  let fail = 0
  console.group('Testing...')
  for (const testModule of resolvedTestModules) {
    const { url, tests } = testModule
    let testUrlPrinted = false
    for (const testName in tests) {
      const test = tests[testName]
      try {
        await test()
        pass += 1
      } catch (error: unknown) {
        const basicInfo =
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

        const assertionInfo =
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
        console.log(
          `%c${basicInfo.message}`,
          'color: crimson'
        )
        console.groupCollapsed('Details')
        if (assertionInfo) {
          console.log(
            '%cActual',
            'color: crimson',
            assertionInfo.actual
          )
          console.log(
            '%cExpected',
            'color: green',
            assertionInfo.expected
          )
          basicInfo.stack = basicInfo.stack?.replace(
            /^Assertion/,
            ''
          )
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

  const end = ctx.now()
  const elapsed = ms(Math.round(end - start))

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
  return fail == 0
}

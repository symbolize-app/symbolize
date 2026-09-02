import gleam/float
import gleam/io
import gleam/list
import gleam/string
import lib_test
import lib_time

pub type Summary {
  Summary(passed: Int, failed: Int)
}

/// Run a statically assembled collection in source order. Test context is
/// captured by each test closure, which is the explicit Gleam replacement for
/// the source runner's lazy Proxy context.
pub fn run_all(
  time: lib_time.Context,
  collection: lib_test.TestCollection,
) -> Summary {
  let start = lib_time.performance_now(time)
  let all_modules = lib_test.modules(collection)
  let only_mode = is_only_mode(all_modules)
  let summary = run_modules(all_modules, only_mode, Summary(0, 0))
  let end = lib_time.performance_now(time)
  print_summary(summary, end -. start)
  summary
}

/// The source runner accepts Promise-returning tests. This continuation form
/// preserves that sequencing for Gleam tests without inventing a Promise
/// value: synchronous tests are lifted by `lib_test.run_async`, and async
/// tests call the next test only after their result arrives.
pub fn run_all_async(
  time: lib_time.Context,
  collection: lib_test.TestCollection,
  done: fn(Summary) -> Nil,
) -> Nil {
  let start = lib_time.performance_now(time)
  let all_modules = lib_test.modules(collection)
  let only_mode = is_only_mode(all_modules)
  run_modules_async(all_modules, only_mode, Summary(0, 0), fn(summary) {
    let end = lib_time.performance_now(time)
    print_summary(summary, end -. start)
    done(summary)
  })
}

/// Run source-shaped, statically registered collections. A collection is
/// callable and may contain nested collections, so the Gleam form keeps the
/// source's module grouping and lazy collection boundary while replacing
/// dynamic imports with ordinary module references. The context factory is
/// called once per selected test, which preserves the source runner's
/// per-test context lifetime without recreating its Proxy.
pub fn run_all_context(
  time: lib_time.Context,
  collections: List(lib_test.ContextTestCollectionModule(context)),
  context_factory: fn() -> lib_test.ContextScope(context),
  done: fn(Summary) -> Nil,
) -> Nil {
  let start = lib_time.performance_now(time)
  let modules = context_modules(collections, [])
  let only_mode = context_is_only_mode(modules)
  run_context_modules(
    modules,
    only_mode,
    context_factory,
    Summary(0, 0),
    fn(summary) {
      let end = lib_time.performance_now(time)
      print_summary(summary, end -. start)
      done(summary)
    },
  )
}

pub fn passed(summary: Summary) -> Int {
  case summary {
    Summary(value, _) -> value
  }
}

pub fn failed(summary: Summary) -> Int {
  case summary {
    Summary(_, value) -> value
  }
}

fn print_summary(summary: Summary, elapsed: Float) -> Nil {
  let Summary(passed, failed) = summary
  io.println("Pass: " <> string.inspect(passed))
  io.println("Fail: " <> string.inspect(failed))
  io.println("Elapsed: " <> string.inspect(float.round(elapsed)) <> "ms")
}

fn is_only_mode(modules: List(lib_test.TestModule)) -> Bool {
  list.any(modules, fn(module) {
    list.any(lib_test.module_tests(module), fn(value) {
      string.starts_with(lib_test.test_name(value), "O:")
    })
  })
}

fn context_is_only_mode(
  modules: List(lib_test.ContextTestModule(context)),
) -> Bool {
  list.any(modules, fn(module) {
    list.any(lib_test.context_module_tests(module), fn(value) {
      string.starts_with(lib_test.context_test_name(value), "O:")
    })
  })
}

fn context_modules(
  collections: List(lib_test.ContextTestCollectionModule(context)),
  output: List(lib_test.ContextTestModule(context)),
) -> List(lib_test.ContextTestModule(context)) {
  case collections {
    [] -> output
    [first, ..rest] ->
      context_modules(
        rest,
        list.append(
          output,
          context_collection_modules(
            lib_test.context_collection_items(
              lib_test.context_collection_module_all(first),
            ),
          ),
        ),
      )
  }
}

fn context_collection_modules(
  items: List(lib_test.ContextCollectionItem(context)),
) -> List(lib_test.ContextTestModule(context)) {
  context_collection_modules_from_items(items, [])
}

fn context_collection_modules_from_items(
  items: List(lib_test.ContextCollectionItem(context)),
  output: List(lib_test.ContextTestModule(context)),
) -> List(lib_test.ContextTestModule(context)) {
  case items {
    [] -> output
    [first, ..rest] ->
      context_collection_modules_from_items(rest, case first {
        lib_test.ContextModule(module) -> list.append(output, [module])
        lib_test.ContextCollection(collection) ->
          list.append(
            output,
            context_collection_modules_from_items(
              lib_test.context_collection_items(collection),
              [],
            ),
          )
      })
  }
}

fn run_context_modules(
  modules: List(lib_test.ContextTestModule(context)),
  only_mode: Bool,
  context_factory: fn() -> lib_test.ContextScope(context),
  summary: Summary,
  done: fn(Summary) -> Nil,
) -> Nil {
  case modules {
    [] -> done(summary)
    [first, ..rest] ->
      run_context_module(
        first,
        only_mode,
        context_factory,
        summary,
        fn(summary) {
          run_context_modules(rest, only_mode, context_factory, summary, done)
        },
      )
  }
}

fn run_context_module(
  module: lib_test.ContextTestModule(context),
  only_mode: Bool,
  context_factory: fn() -> lib_test.ContextScope(context),
  summary: Summary,
  done: fn(Summary) -> Nil,
) -> Nil {
  run_context_tests(
    module,
    lib_test.context_module_tests(module),
    only_mode,
    context_factory,
    summary,
    done,
  )
}

fn run_context_tests(
  module: lib_test.ContextTestModule(context),
  tests: List(lib_test.ContextTest(context)),
  only_mode: Bool,
  context_factory: fn() -> lib_test.ContextScope(context),
  summary: Summary,
  done: fn(Summary) -> Nil,
) -> Nil {
  case tests {
    [] -> done(summary)
    [first, ..rest] -> {
      let selected =
        !only_mode
        || string.starts_with(lib_test.context_test_name(first), "O:")
      case selected {
        False ->
          run_context_tests(
            module,
            rest,
            only_mode,
            context_factory,
            summary,
            done,
          )
        True -> {
          let scope = context_factory()
          lib_test.run_context_async(first, lib_test.context_scope_value(scope))(
            fn(result) {
              case result {
                Error(_) -> {
                  let summary =
                    context_record_result(module, first, result, summary)
                  run_context_tests(
                    module,
                    rest,
                    only_mode,
                    context_factory,
                    summary,
                    done,
                  )
                }
                Ok(_) ->
                  run_context_cleanups(
                    lib_test.context_scope_cleanups(scope),
                    fn(cleanup_result) {
                      let summary =
                        context_record_result(
                          module,
                          first,
                          cleanup_result,
                          summary,
                        )
                      run_context_tests(
                        module,
                        rest,
                        only_mode,
                        context_factory,
                        summary,
                        done,
                      )
                    },
                  )
              }
            },
          )
        }
      }
    }
  }
}

fn run_context_cleanups(
  cleanups: List(fn(fn(Result(Nil, lib_test.Failure)) -> Nil) -> Nil),
  done: fn(lib_test.TestResult) -> Nil,
) -> Nil {
  case cleanups {
    [] -> done(Ok(Nil))
    [first, ..rest] ->
      first(fn(result) {
        case result {
          Error(failure) -> done(Error(failure))
          Ok(_) -> run_context_cleanups(rest, done)
        }
      })
  }
}

fn context_record_result(
  module: lib_test.ContextTestModule(context),
  value: lib_test.ContextTest(context),
  result: lib_test.TestResult,
  summary: Summary,
) -> Summary {
  case result {
    Ok(_) -> {
      io.println(
        "PASS "
        <> lib_test.context_module_url(module)
        <> " "
        <> lib_test.context_test_name(value),
      )
      let Summary(passed, failed) = summary
      Summary(passed + 1, failed)
    }
    Error(failure) -> {
      io.println(
        "FAIL "
        <> lib_test.context_module_url(module)
        <> " "
        <> lib_test.context_test_name(value),
      )
      print_failure_values(failure)
      let Summary(passed, failed) = summary
      Summary(passed, failed + 1)
    }
  }
}

fn print_failure_values(failure: lib_test.Failure) -> Nil {
  case failure {
    lib_test.Assertion(message, actual, expected, _mode) -> {
      io.println(message)
      io.println("Expected: " <> expected)
      io.println("Actual: " <> actual)
    }
    lib_test.FailureError(message) -> io.println(message)
  }
}

fn run_modules(
  modules: List(lib_test.TestModule),
  only_mode: Bool,
  summary: Summary,
) -> Summary {
  case modules {
    [] -> summary
    [first, ..rest] ->
      run_modules(rest, only_mode, run_module(first, only_mode, summary))
  }
}

fn run_modules_async(
  modules: List(lib_test.TestModule),
  only_mode: Bool,
  summary: Summary,
  done: fn(Summary) -> Nil,
) -> Nil {
  case modules {
    [] -> done(summary)
    [first, ..rest] ->
      run_module_async(first, only_mode, summary, fn(summary) {
        run_modules_async(rest, only_mode, summary, done)
      })
  }
}

fn run_module(
  module: lib_test.TestModule,
  only_mode: Bool,
  summary: Summary,
) -> Summary {
  list.fold(lib_test.module_tests(module), summary, fn(summary, value) {
    let selected =
      !only_mode || string.starts_with(lib_test.test_name(value), "O:")
    case selected {
      False -> summary
      True ->
        case lib_test.run_sync(value) {
          Ok(Nil) -> {
            io.println(
              "PASS "
              <> lib_test.module_url(module)
              <> " "
              <> lib_test.test_name(value),
            )
            let Summary(passed, failed) = summary
            Summary(passed + 1, failed)
          }
          Error(failure) -> {
            print_failure(module, value, failure)
            let Summary(passed, failed) = summary
            Summary(passed, failed + 1)
          }
        }
    }
  })
}

fn run_module_async(
  module: lib_test.TestModule,
  only_mode: Bool,
  summary: Summary,
  done: fn(Summary) -> Nil,
) -> Nil {
  run_tests_async(
    module,
    lib_test.module_tests(module),
    only_mode,
    summary,
    done,
  )
}

fn run_tests_async(
  module: lib_test.TestModule,
  tests: List(lib_test.Test),
  only_mode: Bool,
  summary: Summary,
  done: fn(Summary) -> Nil,
) -> Nil {
  case tests {
    [] -> done(summary)
    [value, ..rest] -> {
      let selected =
        !only_mode || string.starts_with(lib_test.test_name(value), "O:")
      case selected {
        False -> run_tests_async(module, rest, only_mode, summary, done)
        True ->
          lib_test.run_async(value)(fn(result) {
            let summary = record_result(module, value, result, summary)
            run_tests_async(module, rest, only_mode, summary, done)
          })
      }
    }
  }
}

fn record_result(
  module: lib_test.TestModule,
  value: lib_test.Test,
  result: lib_test.TestResult,
  summary: Summary,
) -> Summary {
  case result {
    Ok(Nil) -> {
      io.println(
        "PASS "
        <> lib_test.module_url(module)
        <> " "
        <> lib_test.test_name(value),
      )
      let Summary(passed, failed) = summary
      Summary(passed + 1, failed)
    }
    Error(failure) -> {
      print_failure(module, value, failure)
      let Summary(passed, failed) = summary
      Summary(passed, failed + 1)
    }
  }
}

fn print_failure(
  module: lib_test.TestModule,
  value: lib_test.Test,
  failure: lib_test.Failure,
) -> Nil {
  io.println(
    "FAIL " <> lib_test.module_url(module) <> " " <> lib_test.test_name(value),
  )
  case failure {
    lib_test.Assertion(message, actual, expected, _mode) -> {
      io.println(message)
      io.println("Expected: " <> expected)
      io.println("Actual: " <> actual)
    }
    lib_test.FailureError(message) -> io.println(message)
  }
}

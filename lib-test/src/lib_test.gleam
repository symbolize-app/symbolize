import gleam/string
import lib_dataflow as dataflow
import lib_error

/// A source-compatible observation handle for an asynchronous operation.
/// Its mutable state and accessors are implemented in Gleam. The only
/// runtime boundary is scheduling a callback in JavaScript's microtask queue.
pub opaque type SyncPromise(value, reason) {
  SyncPromise(
    context: dataflow.Context,
    state: dataflow.Mutation(SyncPromiseState(value, reason)),
  )
}

type SyncPromiseState(value, reason) {
  Pending
  Resolved(value)
  Rejected(reason)
}

@external(javascript, "./lib_test_microtask_ffi.mjs", "queue_microtask")
fn queue_microtask(callback: fn() -> Nil) -> Nil

@external(javascript, "./lib_test_microtask_ffi.mjs", "raise")
fn raise(value: value) -> a

fn new_sync_promise(
  value: SyncPromiseState(value, reason),
) -> SyncPromise(value, reason) {
  SyncPromise(dataflow.dataflow(), dataflow.state(value))
}

fn read_sync_promise(
  promise: SyncPromise(value, reason),
) -> SyncPromiseState(value, reason) {
  let SyncPromise(_, state) = promise
  state
  |> dataflow.mutation
  |> dataflow.to_computation
  |> dataflow.value
}

fn write_sync_promise(
  promise: SyncPromise(value, reason),
  next: SyncPromiseState(value, reason),
) -> Nil {
  let SyncPromise(context, state) = promise
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, next) })
  Nil
}

fn settle(
  promise: SyncPromise(value, reason),
  result: Result(value, reason),
) -> Nil {
  queue_microtask(fn() {
    case read_sync_promise(promise) {
      Pending -> {
        case result {
          Ok(value) -> write_sync_promise(promise, Resolved(value))
          Error(reason) -> write_sync_promise(promise, Rejected(reason))
        }
      }
      Resolved(_) | Rejected(_) -> Nil
    }
  })
}

/// Observe a callback-based operation without turning it into a blocking
/// operation. This is the Gleam form of the source `sync` helper.
pub fn sync(
  operation: lib_error.Async(value, reason),
) -> SyncPromise(value, reason) {
  let promise = new_sync_promise(Pending)
  operation(fn(result) { settle(promise, result) })
  promise
}

/// The source accepts either a Promise or an already available value. Since
/// Gleam has no value-or-async union, the value form is explicit and retains
/// Promise.resolve's microtask timing.
pub fn sync_value(value: value) -> SyncPromise(value, Nil) {
  let promise = new_sync_promise(Pending)
  queue_microtask(fn() { write_sync_promise(promise, Resolved(value)) })
  promise
}

pub fn is_settled(value: SyncPromise(value, reason)) -> Bool {
  case read_sync_promise(value) {
    Rejected(_) -> True
    Pending | Resolved(_) -> False
  }
}

pub fn resolved_value(value: SyncPromise(value, reason)) -> value {
  case read_sync_promise(value) {
    Rejected(reason) -> raise(reason)
    Pending -> panic as "Promise not resolved yet"
    Resolved(value) -> value
  }
}

pub fn rejected_value(value: SyncPromise(value, reason)) -> reason {
  case read_sync_promise(value) {
    Resolved(_) -> panic as "Promise resolved"
    Pending -> panic as "Promise not rejected yet"
    Rejected(reason) -> reason
  }
}

/// A test is deliberately a zero-argument function. A migrated test captures
/// the context it needs explicitly instead of relying on the source runner's
/// Proxy-based lazy context object.
pub type Test {
  SyncTest(name: String, run: fn() -> TestResult)
  AsyncTest(name: String, run: fn() -> lib_error.Async(Nil, Failure))
}

pub type TestModule {
  TestModule(url: String, tests: List(Test))
}

/// Source test modules that need a typed context. The source gets this
/// context through a lazy Proxy; Gleam keeps the context explicit and builds
/// one value for each test through the runner's context factory.
pub type ContextTest(context) {
  ContextSyncTest(name: String, run: fn(context) -> TestResult)
  ContextAsyncTest(
    name: String,
    run: fn(context) -> lib_error.Async(Nil, Failure),
  )
}

pub type ContextTestModule(context) {
  ContextTestModule(url: String, tests: List(ContextTest(context)))
}

/// A callable static collection preserves the source collection's lazy
/// nesting without requiring dynamic imports or a Proxy-backed context.
pub type ContextTestCollection(context) =
  fn() -> List(ContextCollectionItem(context))

pub type ContextCollectionItem(context) {
  ContextModule(ContextTestModule(context))
  ContextCollection(ContextTestCollection(context))
}

pub type ContextTestCollectionModule(context) {
  ContextTestCollectionModule(all: ContextTestCollection(context))
}

/// A per-test context plus the already-registered cleanup operations. The
/// source runner registers deferred callbacks while building a lazy context
/// and executes them in reverse registration order after a successful test.
/// Gleam keeps that lifetime explicit instead of recreating the source
/// Proxy.
pub type ContextScope(context) {
  ContextScope(value: context, cleanups: List(lib_error.Async(Nil, Failure)))
}

pub type TestCollection {
  TestCollection(items: List(CollectionItem))
}

pub type CollectionItem {
  Module(TestModule)
  Collection(TestCollection)
}

pub type TestResult =
  Result(Nil, Failure)

pub type AssertionMode {
  ErrorMode
  DiffMode
}

pub type Failure {
  Assertion(
    message: String,
    actual: String,
    expected: String,
    mode: AssertionMode,
  )
  FailureError(String)
}

/// A source mock's callback state is explicit in Gleam. Use a tuple for a
/// multi-argument mock, for example `Mock(#(String, Int), Result)`, and thread
/// the returned mock into the next call.
pub opaque type Mock(argument, value) {
  Mock(remaining: List(fn(argument) -> value), history: List(argument))
}

pub fn new_test(name: String, run: fn() -> TestResult) -> Test {
  SyncTest(name, run)
}

pub fn new_async_test(
  name: String,
  run: fn() -> lib_error.Async(Nil, Failure),
) -> Test {
  AsyncTest(name, run)
}

pub fn new_context_test(
  name: String,
  run: fn(context) -> TestResult,
) -> ContextTest(context) {
  ContextSyncTest(name, run)
}

pub fn new_context_async_test(
  name: String,
  run: fn(context) -> lib_error.Async(Nil, Failure),
) -> ContextTest(context) {
  ContextAsyncTest(name, run)
}

pub fn context_module(
  url: String,
  tests: List(ContextTest(context)),
) -> ContextTestModule(context) {
  ContextTestModule(url, tests)
}

pub fn context_collection(
  items: List(ContextCollectionItem(context)),
) -> ContextTestCollection(context) {
  fn() { items }
}

pub fn context_module_item(
  value: ContextTestModule(context),
) -> ContextCollectionItem(context) {
  ContextModule(value)
}

pub fn context_collection_item(
  value: ContextTestCollection(context),
) -> ContextCollectionItem(context) {
  ContextCollection(value)
}

pub fn context_collection_module(
  all: ContextTestCollection(context),
) -> ContextTestCollectionModule(context) {
  ContextTestCollectionModule(all)
}

pub fn context_collection_module_all(
  module: ContextTestCollectionModule(context),
) -> ContextTestCollection(context) {
  case module {
    ContextTestCollectionModule(all) -> all
  }
}

pub fn context_collection_items(
  collection: ContextTestCollection(context),
) -> List(ContextCollectionItem(context)) {
  collection()
}

pub fn context_scope(
  value: context,
  cleanups: List(lib_error.Async(Nil, Failure)),
) -> ContextScope(context) {
  ContextScope(value, cleanups)
}

pub fn context_scope_value(scope: ContextScope(context)) -> context {
  case scope {
    ContextScope(value, _) -> value
  }
}

pub fn context_scope_cleanups(
  scope: ContextScope(context),
) -> List(lib_error.Async(Nil, Failure)) {
  case scope {
    ContextScope(_, cleanups) -> cleanups
  }
}

/// Register a cleanup ahead of earlier registrations. This is the explicit
/// Gleam form of the source runner's `defer` callback, whose `unshift` makes
/// cleanup execution last-in-first-out.
pub fn context_scope_defer(
  scope: ContextScope(context),
  cleanup: lib_error.Async(Nil, Failure),
) -> ContextScope(context) {
  case scope {
    ContextScope(value, cleanups) -> ContextScope(value, [cleanup, ..cleanups])
  }
}

pub fn mock_with_history(
  return_values: List(fn(argument) -> value),
) -> Mock(argument, value) {
  Mock(return_values, [])
}

pub fn repeat_mock_with_history(
  repeat: Int,
  return_value: fn(argument) -> value,
) -> Mock(argument, value) {
  mock_with_history(repeat_values(repeat, return_value, []))
}

pub fn mock_call(
  mock: Mock(argument, value),
  argument: argument,
) -> Result(#(value, Mock(argument, value)), String) {
  case mock {
    Mock([], _) -> Error("called too many times")
    Mock([first, ..rest], history) -> {
      let value = first(argument)
      Ok(#(value, Mock(rest, append(history, [argument]))))
    }
  }
}

pub fn mock_history(mock: Mock(argument, value)) -> List(argument) {
  case mock {
    Mock(_, history) -> history
  }
}

pub fn module(url: String, tests: List(Test)) -> TestModule {
  TestModule(url, tests)
}

pub fn collection(items: List(CollectionItem)) -> TestCollection {
  TestCollection(items)
}

pub fn module_item(value: TestModule) -> CollectionItem {
  Module(value)
}

pub fn collection_item(value: TestCollection) -> CollectionItem {
  Collection(value)
}

/// Flatten nested collections while retaining the source Promise.all order.
pub fn modules(collection: TestCollection) -> List(TestModule) {
  case collection {
    TestCollection(items) -> flatten_items(items)
  }
}

pub fn test_name(value: Test) -> String {
  case value {
    SyncTest(name, _) -> name
    AsyncTest(name, _) -> name
  }
}

pub fn context_test_name(value: ContextTest(context)) -> String {
  case value {
    ContextSyncTest(name, _) -> name
    ContextAsyncTest(name, _) -> name
  }
}

pub fn context_module_url(module: ContextTestModule(context)) -> String {
  case module {
    ContextTestModule(url, _) -> url
  }
}

pub fn context_module_tests(
  module: ContextTestModule(context),
) -> List(ContextTest(context)) {
  case module {
    ContextTestModule(_, tests) -> tests
  }
}

pub fn run_context_async(
  value: ContextTest(context),
  context: context,
) -> lib_error.Async(Nil, Failure) {
  case value {
    ContextSyncTest(_, run) -> fn(done) { done(run(context)) }
    ContextAsyncTest(_, run) -> fn(done) { run(context)(done) }
  }
}

pub fn run_sync(value: Test) -> TestResult {
  case value {
    SyncTest(_, run) -> run()
    AsyncTest(_, _) -> panic as "async test used with run_all"
  }
}

pub fn run_async(value: Test) -> lib_error.Async(Nil, Failure) {
  case value {
    SyncTest(_, run) -> fn(done) { done(run()) }
    AsyncTest(_, run) -> run()
  }
}

pub fn module_url(module: TestModule) -> String {
  case module {
    TestModule(url, _) -> url
  }
}

pub fn module_tests(module: TestModule) -> List(Test) {
  case module {
    TestModule(_, tests) -> tests
  }
}

pub fn assert_true(condition: Bool) -> TestResult {
  case condition {
    True -> Ok(Nil)
    False ->
      Error(Assertion(
        message: "Not truthy",
        actual: string.inspect(condition),
        expected: "(truthy)",
        mode: ErrorMode,
      ))
  }
}

pub fn assert_equal(actual: a, expected: a) -> TestResult {
  case actual == expected {
    True -> Ok(Nil)
    False ->
      Error(Assertion(
        message: "Not equal",
        actual: string.inspect(actual),
        expected: string.inspect(expected),
        mode: ErrorMode,
      ))
  }
}

/// Gleam-owned values already have structural equality. Keeping this separate
/// preserves the source call-site distinction without adding a dynamic deep
/// traversal that cannot inspect foreign Web API values.
pub fn assert_deep_equal(actual: a, expected: a) -> TestResult {
  case actual == expected {
    True -> Ok(Nil)
    False ->
      Error(Assertion(
        message: "Not deep equal",
        actual: string.inspect(actual),
        expected: string.inspect(expected),
        mode: DiffMode,
      ))
  }
}

/// Result values are the explicit Gleam representation of an operation that
/// the TypeScript test API would observe with assertThrows.
pub fn assert_error(result: Result(a, b)) -> Result(b, Failure) {
  case result {
    Error(reason) -> Ok(reason)
    Ok(value) ->
      Error(Assertion(
        message: "No error thrown",
        actual: string.inspect(value),
        expected: "<error>",
        mode: ErrorMode,
      ))
  }
}

/// The continuation form of the source `assertThrowsAsync` helper. A rejected
/// operation becomes the successful assertion result; a fulfilled operation
/// becomes the same assertion failure as `assert_error`.
pub fn assert_error_async(
  operation: lib_error.Async(value, reason),
) -> lib_error.Async(reason, Failure) {
  fn(done) {
    operation(fn(result) {
      case result {
        Error(reason) -> done(Ok(reason))
        Ok(value) ->
          done(
            Error(Assertion(
              message: "No error thrown",
              actual: string.inspect(value),
              expected: "<error>",
              mode: ErrorMode,
            )),
          )
      }
    })
  }
}

pub fn fail(message: String) -> TestResult {
  Error(FailureError(message))
}

fn flatten_items(items: List(CollectionItem)) -> List(TestModule) {
  case items {
    [] -> []
    [first, ..rest] -> append(flatten_item(first), flatten_items(rest))
  }
}

fn repeat_values(
  repeat: Int,
  value: fn(argument) -> value,
  output: List(fn(argument) -> value),
) -> List(fn(argument) -> value) {
  case repeat <= 0 {
    True -> output
    False -> repeat_values(repeat - 1, value, [value, ..output])
  }
}

fn flatten_item(item: CollectionItem) -> List(TestModule) {
  case item {
    Module(module) -> [module]
    Collection(collection) -> modules(collection)
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}

import gleam/io
import lib_test
import lib_test_runner
import lib_time

pub fn main() {
  let collection =
    lib_test.collection([
      lib_test.module_item(
        lib_test.module("runner", [
          lib_test.new_test("pass", fn() { Ok(Nil) }),
          lib_test.new_test("fail", fn() { lib_test.fail("expected failure") }),
        ]),
      ),
    ])
  let summary =
    lib_test_runner.run_all(
      lib_time.new_context(
        lib_time.custom(fn() { 10.0 }, fn(callback, _ms) { callback() }),
      ),
      collection,
    )
  assert lib_test_runner.passed(summary) == 1
  assert lib_test_runner.failed(summary) == 1

  let only_collection =
    lib_test.collection([
      lib_test.module_item(
        lib_test.module("only", [
          lib_test.new_test("ordinary", fn() { panic as "non-only test was run" }),
          lib_test.new_test("O:selected", fn() { Ok(Nil) }),
        ]),
      ),
    ])
  let only_summary =
    lib_test_runner.run_all(
      lib_time.new_context(
        lib_time.custom(fn() { 20.0 }, fn(callback, _ms) { callback() }),
      ),
      only_collection,
    )
  assert lib_test_runner.passed(only_summary) == 1
  assert lib_test_runner.failed(only_summary) == 0

  let async_collection =
    lib_test.collection([
      lib_test.module_item(
        lib_test.module("async", [
          lib_test.new_async_test("callback", fn() {
            fn(done) { done(Ok(Nil)) }
          }),
        ]),
      ),
    ])
  lib_test_runner.run_all_async(
    lib_time.new_context(
      lib_time.custom(fn() { 30.0 }, fn(callback, _ms) { callback() }),
    ),
    async_collection,
    fn(async_summary) {
      assert lib_test_runner.passed(async_summary) == 1
      assert lib_test_runner.failed(async_summary) == 0
    },
  )

  let context_collection =
    lib_test.context_collection_module(
      lib_test.context_collection([
        lib_test.context_module_item(
          lib_test.context_module("context/first", [
            lib_test.new_context_test("sync", fn(context: String) {
              assert context == "fresh"
              Ok(Nil)
            }),
            lib_test.new_context_async_test("async", fn(context: String) {
              fn(done) {
                assert context == "fresh"
                done(Ok(Nil))
              }
            }),
          ]),
        ),
        lib_test.context_collection_item(
          lib_test.context_collection([
            lib_test.context_module_item(
              lib_test.context_module("context/second", [
                lib_test.new_context_test("nested", fn(context: String) {
                  assert context == "fresh"
                  Ok(Nil)
                }),
              ]),
            ),
          ]),
        ),
      ]),
    )
  lib_test_runner.run_all_context(
    lib_time.new_context(
      lib_time.custom(fn() { 40.0 }, fn(callback, _ms) { callback() }),
    ),
    [context_collection],
    fn() { lib_test.context_scope("fresh", []) },
    fn(context_summary) {
      assert lib_test_runner.passed(context_summary) == 3
      assert lib_test_runner.failed(context_summary) == 0
    },
  )

  let cleanup_collection =
    lib_test.context_collection_module(
      lib_test.context_collection([
        lib_test.context_module_item(
          lib_test.context_module("context/cleanup", [
            lib_test.new_context_test("cleanup", fn(_context: String) {
              Ok(Nil)
            }),
          ]),
        ),
      ]),
    )
  lib_test_runner.run_all_context(
    lib_time.new_context(
      lib_time.custom(fn() { 50.0 }, fn(callback, _ms) { callback() }),
    ),
    [cleanup_collection],
    fn() {
      let scope = lib_test.context_scope("fresh", [])
      lib_test.context_scope_defer(scope, fn(done) {
        done(Error(lib_test.FailureError("cleanup failure")))
      })
    },
    fn(cleanup_summary) {
      assert lib_test_runner.passed(cleanup_summary) == 0
      assert lib_test_runner.failed(cleanup_summary) == 1
    },
  )
  io.println("lib-test-runner Gleam parity tests passed")
}

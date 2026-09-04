import gleam/io
import lib_collection_test_main
import lib_dataflow_test_main
import lib_error_test_main
import lib_hex_test_main
import lib_markup_data_test_main
import lib_payload_test_main
import lib_random_test_main
import lib_stream_test_main
import lib_styling_test_main
import lib_time_test_main

// Source: .tmp/symbolize-source/dev-pnpm-test/main.ts and index.ts.
//
// The source aggregate resolves the ten package-level `index.test.ts`
// collections and hands them to lib-test-runner. The migrated package tests
// expose concrete Gleam entrypoints, but each one now has an explicit
// completion continuation. Chaining those continuations preserves source
// collection order and means the aggregate's completion signal is emitted
// only after the callback-based tests have drained.
pub fn run() {
  lib_collection_test_main.run(fn() {
    lib_dataflow_test_main.run(fn() {
      lib_error_test_main.run(fn() {
        lib_hex_test_main.run(fn() {
          lib_markup_data_test_main.run(fn() {
            lib_payload_test_main.run(fn() {
              lib_random_test_main.run(fn() {
                lib_stream_test_main.run(fn() {
                  lib_styling_test_main.run(fn() {
                    lib_time_test_main.run(fn() {
                      io.println("dev-pnpm-test Gleam aggregate completed")
                    })
                  })
                })
              })
            })
          })
        })
      })
    })
  })
}

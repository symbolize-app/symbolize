// This is the development-only equivalent of the source repository's
// svc-gateway-guest-run/main.development.ts -> dev-node-test graph. The test
// suites themselves live with their migrated packages; this module is only
// the static browser entry that keeps that graph discoverable by esbuild.
import gleam/io
import lib_collection_test_main as collection
import lib_dataflow_test_main as dataflow
import lib_error_test_main as error
import lib_hex_test_main as hex
import lib_markup_browser_test_main as markup
import lib_payload_test_main as payload
import lib_random_test_main as random
import lib_stream_test_main as stream
import lib_styling_test_main as styling
import lib_time_test_main as time

pub fn main() {
  collection.main()
  dataflow.main()
  error.main()
  hex.main()
  markup.main()
  payload.main()
  random.main()
  stream.main()
  styling.main()
  time.main()
  io.println("development Gleam test graph loaded")
}

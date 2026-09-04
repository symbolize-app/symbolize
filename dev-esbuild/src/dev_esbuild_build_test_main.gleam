import dev_esbuild_build as build
import dev_esbuild_modules as modules
import dev_esbuild_node as node
import gleam/bit_array
import gleam/io
import gleam/string

type Fixture {
  Fixture(
    entry: String,
    outbase: String,
    outdir: String,
    copy_html: String,
    copy_font: String,
    classic_worker: String,
  )
}

fn fixture() -> Fixture {
  let directory = node.temporary_directory("symbolize-dev-esbuild-")
  let entry = directory <> "/entry.js"
  let dependency = directory <> "/dependency.js"
  let copy_html = directory <> "/init.html"
  let copy_font = directory <> "/font.woff2"
  let classic_worker = directory <> "/serviceWorker.js"
  node.write_file(
    entry,
    bit_array.from_string(
      "import { dependency } from \"./dependency.js\"; export const result = dependency;\n",
    ),
  )
  node.write_file(
    dependency,
    bit_array.from_string("export const dependency = 42;\n"),
  )
  node.write_file(copy_html, bit_array.from_string("<!doctype html>\n"))
  node.write_file(copy_font, <<0, 1, 2, 3>>)
  node.write_file(
    classic_worker,
    bit_array.from_string("self.addEventListener('install', () => {});\n"),
  )
  Fixture(
    entry,
    directory,
    directory <> "/out",
    copy_html,
    copy_font,
    classic_worker,
  )
}

pub fn main() {
  let Fixture(entry, outbase, outdir, _, _, _) = fixture()
  let resolver = modules.resolver(modules.context(outbase), [entry])
  build.build_modules([entry], outbase, outdir, False, resolver)(fn(result) {
    case result {
      Error(reason) -> panic as reason
      Ok(#(errors, files, warnings)) -> {
        assert errors == []
        assert warnings == []
        assert has_file(files, "entry.js.mjs")
        assert has_file(files, "dependency.js.mjs")
        assert imported_dependency(files)
      }
    }
    node.remove(outbase)(fn(result) {
      assert result == Ok(Nil)
      io.println("dev-esbuild esbuild resolver parity passed")
    })
  })

  let Fixture(_, copy_outbase, copy_outdir, copy_html, copy_font, _) = fixture()
  build.build_copy([copy_html, copy_font], copy_outbase, copy_outdir, False)(
    fn(result) {
      case result {
        Error(reason) -> panic as reason
        Ok(#(errors, files, warnings)) -> {
          assert errors == []
          assert warnings == []
          assert has_file(files, "init.html")
          assert has_file(files, "font.woff2")
        }
      }
      node.remove(copy_outbase)(fn(result) {
        assert result == Ok(Nil)
        io.println("dev-esbuild esbuild copy parity passed")
      })
    },
  )

  let Fixture(_, classic_outbase, classic_outdir, _, _, classic_worker) =
    fixture()
  build.build_classic([classic_worker], classic_outbase, classic_outdir, False)(
    fn(result) {
      case result {
        Error(reason) -> panic as reason
        Ok(#(errors, files, warnings)) -> {
          assert errors == []
          assert warnings == []
          assert has_file(files, "serviceWorker.js")
        }
      }
      node.remove(classic_outbase)(fn(result) {
        assert result == Ok(Nil)
        io.println("dev-esbuild esbuild classic parity passed")
      })
    },
  )

  let Fixture(
    all_entry,
    all_outbase,
    all_outdir,
    all_html,
    all_font,
    all_worker,
  ) = fixture()
  let all_resolver = modules.resolver(modules.context(all_outbase), [all_entry])
  build.build_all(
    [all_html, all_font],
    [all_worker],
    [all_entry],
    all_outbase,
    all_outdir,
    False,
    all_resolver,
  )(fn(result) {
    case result {
      Error(reason) -> panic as reason
      Ok(#(copy, classic, module)) -> {
        assert has_file(copy.1, "init.html")
        assert has_file(copy.1, "font.woff2")
        assert has_file(classic.1, "serviceWorker.js")
        assert has_file(module.1, "entry.js.mjs")
        assert has_file(module.1, "dependency.js.mjs")
      }
    }
    node.remove(all_outbase)(fn(result) {
      assert result == Ok(Nil)
      io.println("dev-esbuild concurrent build parity passed")
    })
  })
}

fn has_file(files: List(#(BitArray, String)), suffix: String) -> Bool {
  case files {
    [] -> False
    [#(_, path), ..rest] ->
      case string.ends_with(path, suffix) {
        True -> True
        False -> has_file(rest, suffix)
      }
  }
}

fn imported_dependency(files: List(#(BitArray, String))) -> Bool {
  case files {
    [] -> False
    [#(contents, path), ..rest] ->
      case string.ends_with(path, "entry.js.mjs") {
        True ->
          case bit_array.to_string(contents) {
            Ok(text) -> string.contains(text, "./dependency.js.mjs")
            Error(_) -> False
          }
        False -> imported_dependency(rest)
      }
  }
}

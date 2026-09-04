const { build: esbuildBuild } = await import(
  new URL(
    '../../../../../vendor/esbuild-0.19.5/symbolize.mjs',
    import.meta.url,
  )
)
import { List, Result$Error, Result$Ok, toBitArray } from './gleam.mjs'

const resolveBase = Symbol('resolveBase')

export function new_options() {
  return { define: {}, loader: {}, outExtension: {} }
}

export function set_bundle(options, value) {
  options.bundle = value
}

export function set_entry_points(options, entryPoints) {
  options.entryPoints = entryPoints.toArray()
}

export function set_outbase(options, value) {
  options.outbase = value
}

export function set_outdir(options, value) {
  options.outdir = value
}

export function set_define(options, key, value) {
  options.define[key] = value
}

export function set_external(options, values) {
  options.external = values.toArray()
}

export function set_log_level(options, value) {
  options.logLevel = value
}

export function set_minify(options, value) {
  options.minify = value
}

export function set_platform(options, value) {
  options.platform = value
}

export function set_write(options, value) {
  options.write = value
}

export function set_loader(options, extension, value) {
  options.loader[extension] = value
}

export function set_format(options, value) {
  options.format = value
}

export function set_entry_names(options, value) {
  options.entryNames = value
}

export function set_footer(options, value) {
  options.footer = { js: value }
}

export function set_global_name(options, value) {
  options.globalName = value
}

export function set_out_extension(options, extension, value) {
  options.outExtension[extension] = value
}

export function set_plugins(options, plugins) {
  options.plugins = plugins.toArray()
}

export function new_entry_point(input, output) {
  return { in: input, out: output }
}

export function new_modules_plugin(rewrite) {
  const plugin = {
    name: 'buildModules',
    setup(buildApi) {
      buildApi.onResolve({ filter: /.*/ }, async (args) => {
        if (args.pluginData === resolveBase) return undefined
        const resolveResult = await buildApi.resolve(args.path, {
          importer: args.importer,
          kind: args.kind,
          namespace: args.namespace,
          pluginData: resolveBase,
          resolveDir: args.resolveDir,
        })
        const rewritten = rewrite(args, resolveResult)
        if (!rewritten.isOk()) throw new Error(rewritten[0])
        const path = rewritten[0]?.[0]
        if (path !== undefined) {
          return {
            ...resolveResult,
            external: true,
            namespace: 'buildModules',
            path,
          }
        }
        return resolveResult
      })
    },
  }
  return plugin
}

export function build(options, done) {
  esbuildBuild(options).then(
    (result) => done(Result$Ok(result)),
    (error) => done(Result$Error(String(error))),
  )
}

export function result_errors(result) {
  return List.fromArray(result.errors)
}

export function result_output_files(result) {
  return List.fromArray(result.outputFiles ?? [])
}

export function result_warnings(result) {
  return List.fromArray(result.warnings)
}

export function message_text(message) {
  return message.text
}

export function output_file_contents(file) {
  return toBitArray([file.contents])
}

export function output_file_path(file) {
  return file.path
}

export function resolve_args_importer(args) {
  return args.importer
}

export function resolve_args_kind(args) {
  return args.kind
}

export function resolve_result_external(result) {
  return result.external
}

export function resolve_result_has_errors(result) {
  return result.errors.length !== 0
}

export function resolve_result_path(result) {
  return result.path
}

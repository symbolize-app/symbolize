import * as esbuild from 'esbuild'
import * as nodePath from 'node:path'

export interface BuildOptions
  extends Omit<
    esbuild.BuildOptions,
    | 'metafile'
    | 'mangleCache'
    | 'entryPoints'
    | 'stdin'
    | 'bundle'
    | 'outbase'
    | 'outExtensions'
  > {
  entryPoints: string[]
  outbase: string
}

export interface BuildResult<
  ProvidedOptions extends BuildOptions = BuildOptions,
> extends Omit<
    esbuild.BuildResult<ProvidedOptions>,
    'metafile' | 'mangleCache' | 'outputFiles'
  > {
  outputFiles: esbuild.OutputFile[]
}

export async function build<T extends BuildOptions>(
  options: esbuild.SameShape<BuildOptions, T>
): Promise<BuildResult<T>> {
  const result: BuildResult<T> = {
    errors: [],
    warnings: [],
    outputFiles: [],
  }
  const allEntryPoints = new Set(options.entryPoints)
  let entryPoints = options.entryPoints.map(
    (entryPoint) => ({
      in: entryPoint,
      out: nodePath.relative(options.outbase, entryPoint),
    })
  )
  while (entryPoints.length) {
    const newEntryPoints: { in: string; out: string }[] = []
    const plugin: esbuild.Plugin = {
      name: 'buildModules',
      setup(build) {
        build.onResolve({ filter: /.*/ }, async (args) => {
          if (args.pluginData === true) {
            return undefined
          }
          const resolveResult = await build.resolve(
            args.path,
            {
              importer: args.importer,
              namespace: args.namespace,
              resolveDir: args.resolveDir,
              kind: args.kind,
              pluginData: true,
            }
          )
          if (
            !resolveResult.errors.length &&
            !resolveResult.external &&
            ['import-statement', 'dynamic-import'].includes(
              args.kind
            )
          ) {
            if (!allEntryPoints.has(resolveResult.path)) {
              newEntryPoints.push({
                in: resolveResult.path,
                out: nodePath.relative(
                  options.outbase,
                  resolveResult.path
                ),
              })
              allEntryPoints.add(resolveResult.path)
            }
            const relativePath = `${nodePath.relative(
              nodePath.dirname(args.importer),
              resolveResult.path
            )}.mjs`
            return {
              ...resolveResult,
              path: relativePath.startsWith('.')
                ? relativePath
                : `./${relativePath}`,
              namespace: 'buildModules',
              external: true,
            }
          } else {
            return resolveResult
          }
        })
      },
    }
    const moduleResult = await esbuild.build({
      ...options,
      bundle: true,
      entryPoints,
      outExtension: { ['.js']: '.mjs' },
      plugins: [...(options.plugins ?? []), plugin],
    })
    result.errors.push(...moduleResult.errors)
    result.warnings.push(...moduleResult.warnings)
    result.outputFiles.push(
      ...(moduleResult.outputFiles ?? [])
    )
    entryPoints = newEntryPoints
  }
  return result
}

import * as collection from '@intertwine/lib-collection'
import * as esbuild from 'esbuild'
import * as nodePath from 'node:path'

export interface BuildOptions
  extends Omit<
    esbuild.BuildOptions,
    | 'bundle'
    | 'entryPoints'
    | 'mangleCache'
    | 'metafile'
    | 'outbase'
    | 'outExtensions'
    | 'stdin'
  > {
  entryPoints: string[]
  outbase: string
}

export interface BuildResult<
  ProvidedOptions extends BuildOptions = BuildOptions,
> extends Omit<
    esbuild.BuildResult<ProvidedOptions>,
    'mangleCache' | 'metafile' | 'outputFiles'
  > {
  outputFiles: esbuild.OutputFile[]
}

const resolveBase = Symbol('resolveBase')

export async function build<T extends BuildOptions>(
  options: esbuild.SameShape<BuildOptions, T>
): Promise<BuildResult<T>> {
  const result: BuildResult<T> = {
    errors: [],
    outputFiles: [],
    warnings: [],
  }

  const pnpmPackageVersions = new Map<string, string>()
  const convertToOutPathMemo = new collection.Memo((inPath: string) =>
    convertToOutPath(inPath, options.outbase, pnpmPackageVersions)
  )
  const allEntryPoints = new Set(options.entryPoints)
  let entryPoints = options.entryPoints.map((entryPoint) => ({
    in: entryPoint,
    out: convertToOutPathMemo.get(entryPoint),
  }))

  while (entryPoints.length) {
    const newEntryPoints: { in: string; out: string }[] = []

    const plugin: esbuild.Plugin = {
      name: 'buildModules',
      setup(build) {
        build.onResolve({ filter: /.*/ }, async (args) =>
          resolve(
            build,
            convertToOutPathMemo,
            allEntryPoints,
            newEntryPoints,
            args
          )
        )
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
    result.outputFiles.push(...(moduleResult.outputFiles ?? []))
    entryPoints = newEntryPoints
  }

  return result
}

async function resolve(
  build: esbuild.PluginBuild,
  convertToOutPathMemo: collection.Memo<string, string>,
  allEntryPoints: Set<string>,
  newEntryPoints: { in: string; out: string }[],
  args: esbuild.OnResolveArgs
): Promise<esbuild.OnResolveResult | undefined> {
  if (args.pluginData === resolveBase) {
    return undefined
  }
  const resolveResult = await build.resolve(args.path, {
    importer: args.importer,
    kind: args.kind,
    namespace: args.namespace,
    pluginData: resolveBase,
    resolveDir: args.resolveDir,
  })
  if (
    !resolveResult.errors.length &&
    !resolveResult.external &&
    ['import-statement', 'dynamic-import'].includes(args.kind)
  ) {
    const outPath = convertToOutPathMemo.get(resolveResult.path)
    if (!allEntryPoints.has(resolveResult.path)) {
      newEntryPoints.push({
        in: resolveResult.path,
        out: outPath,
      })
      allEntryPoints.add(resolveResult.path)
    }
    const relativePath = `${nodePath.relative(
      nodePath.dirname(convertToOutPathMemo.get(args.importer)),
      outPath
    )}.mjs`
    return {
      ...resolveResult,
      external: true,
      namespace: 'buildModules',
      path: relativePath.startsWith('.')
        ? relativePath
        : `./${relativePath}`,
    }
  } else {
    return resolveResult
  }
}

const pnpmPattern = new RegExp(
  /^node_modules\/\.pnpm\/(?<package_>[^/]+)@(?<version>[^/]+)\/node_modules\/[^/]+\//
)

function convertToOutPath(
  inPath: string,
  outbase: string,
  pnpmPackageVersions: Map<string, string>
): string {
  const relative = nodePath.relative(outbase, inPath)
  const pnpmMatch = pnpmPattern.exec(relative)
  if (!pnpmMatch) {
    return relative
  } else {
    const { package_, version } = pnpmMatch.groups as {
      package_: string
      version: string
    }
    const previousVersion = pnpmPackageVersions.get(package_)
    if (previousVersion === undefined) {
      pnpmPackageVersions.set(package_, version)
    } else if (previousVersion !== version) {
      throw new Error(
        `Ambiguous versions found for ${package_}: ${previousVersion} / ${version}`
      )
    }
    return `.pnpm-${package_}/${relative.substring(pnpmMatch[0].length)}`
  }
}

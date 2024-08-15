import * as collection from '@intertwine/lib-collection'
import * as esbuild from 'esbuild'
import * as nodePath from 'node:path'

// eslint-disable-next-line functional/type-declaration-immutability -- library compatibility
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

// eslint-disable-next-line functional/type-declaration-immutability -- library compatibility
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
  // eslint-disable-next-line functional/prefer-immutable-types -- library compatibility
  options: esbuild.SameShape<BuildOptions, T>,
): Promise<BuildResult<T>> {
  const mutableResult: BuildResult<T> = {
    errors: [],
    outputFiles: [],
    warnings: [],
  }

  const pnpmPackageVersions = new Map<string, string>()
  const convertToOutPathMemo = collection.memo((inPath: string) =>
    convertToOutPath(inPath, options.outbase, pnpmPackageVersions),
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
            args,
          ),
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

    mutableResult.errors.push(...moduleResult.errors)
    mutableResult.warnings.push(...moduleResult.warnings)
    mutableResult.outputFiles.push(...(moduleResult.outputFiles ?? []))
    entryPoints = newEntryPoints
  }

  return mutableResult
}

async function resolve(
  build: Readonly<esbuild.PluginBuild>,
  convertToOutPathMemo: collection.Memo<string, string>,
  mutableAllEntryPoints: Set<string>,
  mutableNewEntryPoints: { in: string; out: string }[],
  args: Readonly<esbuild.OnResolveArgs>,
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
    if (!mutableAllEntryPoints.has(resolveResult.path)) {
      mutableNewEntryPoints.push({
        in: resolveResult.path,
        out: outPath,
      })
      mutableAllEntryPoints.add(resolveResult.path)
    }
    const relativePath = `${nodePath.relative(
      nodePath.dirname(convertToOutPathMemo.get(args.importer)),
      outPath,
    )}.mjs`
    return {
      ...resolveResult,
      external: true,
      namespace: 'buildModules',
      path:
        relativePath.startsWith('.') ? relativePath : `./${relativePath}`,
    }
  } else {
    return resolveResult
  }
}

const pnpmPattern: Readonly<RegExp> = new RegExp(
  /^node_modules\/\.pnpm\/(?<package_>[^/]+)@(?<version>[^/]+)\/node_modules\/[^/]+\//,
)

function convertToOutPath(
  inPath: string,
  outbase: string,
  mutablePnpmPackageVersions: Map<string, string>,
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
    const previousVersion = mutablePnpmPackageVersions.get(package_)
    if (previousVersion === undefined) {
      mutablePnpmPackageVersions.set(package_, version)
    } else if (previousVersion !== version) {
      throw new Error(
        `Ambiguous versions found for ${package_}: ${previousVersion} / ${version}`,
      )
    }
    return `.pnpm-${package_}/${relative.substring(pnpmMatch[0].length)}`
  }
}

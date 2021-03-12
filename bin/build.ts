import esbuild from 'esbuild'
import * as fsPromises from 'fs/promises'
import module from 'module'
import * as pathModule from 'path'
import sodium from 'sodium-native'
import * as urlModule from 'url'

async function main(): Promise<void> {
  await fsPromises.rm('build', {
    recursive: true,
    force: true,
  })
  await fsPromises.mkdir('build/browser', {
    recursive: true,
  })
  await fsPromises.copyFile(
    'public/index.html',
    'build/browser/index.html'
  )
  await all({
    entryPoints: [
      await import.meta.resolve('@fe/ui/index.ts'),
    ],
    platform: 'browser',
  })
  await fsPromises.mkdir('build/node', { recursive: true })
  await fsPromises.writeFile(
    'build/node/package.json',
    JSON.stringify({
      type: 'module',
    })
  )
  await all({
    entryPoints: [
      await import.meta.resolve('@fe/api/index.ts'),
    ],
    platform: 'node',
  })
  console.log('Done build')
}

type Platform = 'browser' | 'node'

type BuildAllOptions = {
  entryPoints: string[]
  platform: Platform
}

export type BuildAllResult = {
  tree: SourceTree
}

export type SourceTree = Record<string, SourceFile>

export type SourceFile = SourceFileRef & {
  contents: Buffer
}

export type SourceFileRef = {
  path: string
  hash: string
}

export type BuildOptions = {
  entryPoint: string
  platform: Platform
  write?: boolean
}

export type BuildResult = {
  nextSteps: string[]
}

const localRootPath = pathModule.resolve(
  urlModule.fileURLToPath(import.meta.url),
  '..',
  '..'
)

async function all(
  options: BuildAllOptions
): Promise<void> {
  const completedSteps: Set<string> = new Set()
  const nextSteps: string[] = [...options.entryPoints]
  // eslint-disable-next-line no-constant-condition
  while (true) {
    const step = nextSteps.pop()
    if (!step) {
      break
    } else if (completedSteps.has(step)) {
      continue
    }
    const result = await oneStep({
      entryPoint: step,
      platform: options.platform,
    })
    completedSteps.add(step)
    nextSteps.push(...result.nextSteps)
  }
}

export async function oneStep(
  options: BuildOptions & { write: false }
): Promise<BuildResult & { output: SourceFile }>
export async function oneStep(
  options: BuildOptions
): Promise<BuildResult>
export async function oneStep(
  options: BuildOptions
): Promise<BuildResult & { output?: SourceFile }> {
  const [fullEntryPointPath, outfile] = getBuildPaths(
    options
  )
  const nextSteps: string[] = []
  try {
    const result = await esbuild.build({
      bundle: true,
      define: {
        ['import.meta.env.NODE_ENV']: JSON.stringify(
          'development'
        ),
      },
      entryPoints: [fullEntryPointPath],
      format: 'esm',
      outfile,
      platform: options.platform,
      plugins: [
        {
          name: 'custom',
          setup: plugin,
        },
      ],
      write: options.write,
    })
    let output: SourceFile | undefined
    if (result.outputFiles) {
      const contents = Buffer.from(
        result.outputFiles[0].contents
      )
      const hash = Buffer.alloc(
        sodium.crypto_generichash_BYTES
      )
      sodium.crypto_generichash(hash, contents)
      output = {
        path: outfile,
        contents,
        hash: hash.toString('hex'),
      }
    }
    return { nextSteps, output }
  } catch {
    process.exit(1)
  }

  function plugin(build: esbuild.PluginBuild) {
    build.onResolve({ filter: /.*/ }, async (args) => {
      if (args.kind === 'entry-point') {
        return undefined
      } else if (
        args.kind === 'import-statement' ||
        args.kind === 'dynamic-import'
      ) {
        const url = await import.meta.resolve(
          args.path,
          urlModule.pathToFileURL(args.importer).toString()
        )
        if (url.startsWith('node:')) {
          return { path: args.path, external: true }
        }
        const fullPath = urlModule.fileURLToPath(url)
        const localPath = pathModule.relative(
          localRootPath,
          fullPath
        )
        if (localPath.startsWith(`..${pathModule.sep}`)) {
          throw new Error(`Invalid path ${args.path}`)
        } else if (
          options.platform === 'node' &&
          localPath.startsWith('node_modules/')
        ) {
          return { path: args.path, external: true }
        }
        let path = `${pathModule.relative(
          pathModule.join(args.importer, '..'),
          fullPath
        )}.mjs`
        if (!path.startsWith(`.${pathModule.sep}`)) {
          path = `.${pathModule.sep}${path}`
        }
        nextSteps.push(url)
        return { path, external: true }
      } else if (options.platform === 'node') {
        throw new Error(`Unsupported resolve ${args.kind}`)
      } else {
        const fullPath = module
          .createRequire(args.importer)
          .resolve(args.path)
        if (!pathModule.isAbsolute(fullPath)) {
          throw new Error(`Invalid path ${args.path}`)
        }
        return { path: fullPath, namespace: 'file' }
      }
    })
  }
}

function getBuildPaths(
  options: BuildOptions
): [fullEntryPointPath: string, outfile: string] {
  const fullEntryPointPath = urlModule.fileURLToPath(
    options.entryPoint
  )
  const localEntryPointPath = pathModule.relative(
    localRootPath,
    fullEntryPointPath
  )
  return [
    fullEntryPointPath,
    `${pathModule.join(
      options.platform === 'browser'
        ? 'build/browser/js'
        : 'build/node',
      localEntryPointPath
    )}.mjs`,
  ]
}

export function getOutputPath(
  options: BuildOptions
): string {
  return getBuildPaths(options)[1]
}

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void main()
}

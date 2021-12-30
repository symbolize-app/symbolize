import * as crypto from '@tiny/core/crypto.node.ts'
import esbuild from 'esbuild'
import * as fsPromises from 'node:fs/promises'
import module from 'node:module'
import * as pathModule from 'node:path'
import * as urlModule from 'node:url'

async function main(): Promise<void> {
  await fsPromises.rm('build', {
    recursive: true,
    force: true,
  })
  await fsPromises.mkdir('build/browser', {
    recursive: true,
  })
  await fsPromises.copyFile(
    'app/public/index.html',
    'build/browser/index.html'
  )
  await all({
    entryPoints: [
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      await import.meta.resolve!('@fe/ui/index.ts'),
    ],
    platform: 'browser',
    define: {
      ['import.meta.env.NODE_ENV']:
        JSON.stringify('production'),
    },
  })
  await fsPromises.mkdir('build/node', { recursive: true })
  await all({
    entryPoints: [
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      await import.meta.resolve!('@fe/api/index.ts'),
    ],
    platform: 'node',
    define: {
      ['import.meta.env.NODE_ENV']:
        JSON.stringify('production'),
    },
  })
  console.log('Done build')
}

type Platform = 'browser' | 'node'

type BuildAllOptions = {
  entryPoints: string[]
  platform: Platform
  define: Record<string, string>
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
  define: Record<string, string>
  write?: boolean
}

export type BuildResult = {
  nextSteps: string[]
}

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
      define: options.define,
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
  const [fullEntryPointPath, outfile] =
    getBuildPaths(options)
  const nextSteps: string[] = []
  try {
    const result = await esbuild.build({
      bundle: true,
      define: options.define,
      entryPoints: [fullEntryPointPath],
      format: 'esm',
      loader: {
        '.sql': 'text',
      },
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
      const hash = crypto.hash(contents)
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
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        let url = await import.meta.resolve!(
          args.path,
          urlModule.pathToFileURL(args.importer).toString()
        )
        if (
          options.platform === 'browser' &&
          url.endsWith('/lodash-es/isBuffer.js')
        ) {
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          url = await import.meta.resolve!(
            'lodash-es/stubFalse.js',
            urlModule
              .pathToFileURL(args.importer)
              .toString()
          )
        }
        if (url.startsWith('node:')) {
          return { path: args.path, external: true }
        }
        const fullPath = urlModule.fileURLToPath(url)
        const localPath = pathModule.relative('.', fullPath)
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
          return { path: args.path, external: true }
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
    '.',
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

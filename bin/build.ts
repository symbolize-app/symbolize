import esbuild from 'esbuild'
import * as fsPromises from 'fs/promises'
import module from 'module'
import * as pathModule from 'path'
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
  await buildAll({
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
  await buildAll({
    entryPoints: [
      await import.meta.resolve('@fe/api/serve.ts'),
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

type BuildOptions = {
  entryPoint: string
  platform: Platform
  write?: boolean
}

type BuildResult = {
  nextSteps: string[]
}

type OutputFile = {
  path: string
  contents: Uint8Array
}

const localRootPath = pathModule.resolve(
  urlModule.fileURLToPath(import.meta.url),
  '..',
  '..'
)

async function buildAll(
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
    const result = await build({
      entryPoint: step,
      platform: options.platform,
    })
    completedSteps.add(step)
    nextSteps.push(...result.nextSteps)
  }
}

async function build(
  options: BuildOptions & { write: false }
): Promise<BuildResult & { outputFiles: OutputFile[] }>
async function build(
  options: BuildOptions
): Promise<BuildResult>
async function build(
  options: BuildOptions
): Promise<BuildResult & { outputFiles?: OutputFile[] }> {
  const fullEntryPointPath = urlModule.fileURLToPath(
    options.entryPoint
  )
  const localEntryPointPath = pathModule.relative(
    localRootPath,
    fullEntryPointPath
  )
  const outfile = pathModule
    .join(
      options.platform === 'browser'
        ? 'build/browser/js'
        : 'build/node',
      localEntryPointPath
    )
    .replace(/\.[^.]+$/, '.mjs')
  const nextSteps: string[] = []
  try {
    const result = await esbuild.build({
      bundle: true,
      define: {
        ['import.meta.env.MODE']: JSON.stringify(
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
    return { nextSteps, outputFiles: result.outputFiles }
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
        }
        let path = pathModule
          .relative(
            pathModule.join(args.importer, '..'),
            fullPath
          )
          .replace(/\.[^.]+$/, '.mjs')
        if (!path.startsWith(`.${pathModule.sep}`)) {
          path = `.${pathModule.sep}${path}`
        }
        nextSteps.push(url)
        return { path, external: true }
      } else {
        const fullPath = module
          .createRequire(args.importer)
          .resolve(args.path)
        if (!pathModule.isAbsolute(fullPath)) {
          return {
            path: args.path,
            namespace: 'external',
          }
        }
        return { path: fullPath, namespace: 'file' }
      }
    })
  }
}

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void main()
}

import { readFileSync, existsSync } from 'node:fs'
import { fileURLToPath, pathToFileURL } from 'node:url'
import { dirname, resolve as pathResolve, extname } from 'node:path'
import { createRequire } from 'node:module'

const require = createRequire(import.meta.url)

const currentDir = dirname(fileURLToPath(import.meta.url))
const repoRoot = process.env.DEVENV_ROOT ?? pathResolve(currentDir, '..')
process.env.ESBUILD_BINARY_PATH ??= pathResolve(
  repoRoot,
  'build/vendor/esbuild/esbuild',
)

let mappings = null

function loadMappings() {
  if (mappings) return mappings
  const nodeJsonPath = pathResolve(repoRoot, 'vendor/node.json')
  const nodeJson = JSON.parse(readFileSync(nodeJsonPath, 'utf8'))
  mappings = new Map()
  for (const [spec, relPath] of Object.entries(nodeJson.imports || {})) {
    mappings.set(spec, pathResolve(repoRoot, relPath))
  }
  return mappings
}

let transformSync = null

function getTransformSync() {
  if (!transformSync) {
    const esbuildBin = pathResolve(
      repoRoot,
      'build/vendor/esbuild/esbuild',
    )
    process.env.ESBUILD_BINARY_PATH ??= esbuildBin
    const esbuildMain = pathResolve(
      repoRoot,
      'build/vendor/esbuild/main.js',
    )
    const esbuildMod = require(esbuildMain)
    transformSync = esbuildMod.transformSync
  }
  return transformSync
}

export async function resolve(specifier, context, nextResolve) {
  const map = loadMappings()

  if (map.has(specifier)) {
    return {
      url: pathToFileURL(map.get(specifier)).href,
      shortCircuit: true,
    }
  }

  // Handle prefix mappings
  for (const [key, target] of map) {
    if (specifier.startsWith(key + '/')) {
      const subpath = specifier.slice(key.length + 1)
      let baseDir = target
      if (extname(target)) {
        let dir = dirname(target)
        while (
          dir !== repoRoot &&
          !existsSync(pathResolve(dir, 'package.json'))
        ) {
          dir = dirname(dir)
        }
        baseDir = dir
      }
      let candidate = pathResolve(baseDir, subpath)
      if (!existsSync(candidate)) {
        if (candidate.endsWith('.js')) {
          const tsCandidate = candidate.slice(0, -3) + '.ts'
          const dtsCandidate = candidate.slice(0, -3) + '.d.ts'
          if (existsSync(tsCandidate)) candidate = tsCandidate
          else if (existsSync(dtsCandidate)) candidate = dtsCandidate
        } else if (existsSync(candidate + '.ts')) {
          candidate = candidate + '.ts'
        } else if (existsSync(candidate + '.js')) {
          candidate = candidate + '.js'
        } else if (existsSync(pathResolve(candidate, 'index.ts'))) {
          candidate = pathResolve(candidate, 'index.ts')
        } else if (existsSync(pathResolve(candidate, 'index.js'))) {
          candidate = pathResolve(candidate, 'index.js')
        }
      }
      if (existsSync(candidate)) {
        return {
          url: pathToFileURL(candidate).href,
          shortCircuit: true,
        }
      }
    }
  }

  // Handle generated/injected.js for puppeteer
  if (specifier.includes('generated/injected.js')) {
    const injectedPath = pathResolve(
      repoRoot,
      'build/vendor/puppeteer/injected.js',
    )
    return {
      url: pathToFileURL(injectedPath).href,
      shortCircuit: true,
    }
  }

  // Relative import resolution: .js -> .ts or extensionless -> .ts / /index.ts
  if (specifier.startsWith('.') && context.parentURL) {
    const targetUrl = new URL(specifier, context.parentURL)
    const targetPath = fileURLToPath(targetUrl)
    if (!existsSync(targetPath)) {
      if (specifier.endsWith('.js')) {
        const tsPath = targetPath.slice(0, -3) + '.ts'
        if (existsSync(tsPath)) {
          return {
            url: pathToFileURL(tsPath).href,
            shortCircuit: true,
          }
        }
      } else {
        if (existsSync(targetPath + '.ts')) {
          return {
            url: pathToFileURL(targetPath + '.ts').href,
            shortCircuit: true,
          }
        }
        if (existsSync(targetPath + '.js')) {
          return {
            url: pathToFileURL(targetPath + '.js').href,
            shortCircuit: true,
          }
        }
        if (existsSync(pathResolve(targetPath, 'index.ts'))) {
          return {
            url: pathToFileURL(pathResolve(targetPath, 'index.ts')).href,
            shortCircuit: true,
          }
        }
        if (existsSync(pathResolve(targetPath, 'index.js'))) {
          return {
            url: pathToFileURL(pathResolve(targetPath, 'index.js')).href,
            shortCircuit: true,
          }
        }
      }
    }
  }

  return nextResolve(specifier, context)
}

function robustStrip(source) {
  let result = ''
  let i = 0
  const n = source.length
  while (i < n) {
    let wsStart = i
    let isLineStart = i === 0 || source[i - 1] === '\n'
    while (i < n && (source[i] === ' ' || source[i] === '\t')) i++
    if (i < n && source[i] === '@' && (isLineStart || wsStart < i)) {
      i++ // skip @
      let identStart = i
      while (i < n && /[a-zA-Z0-9_$]/.test(source[i])) i++
      if (i > identStart) {
        if (i < n && source[i] === '<') {
          let depth = 1
          i++
          while (i < n && depth > 0) {
            if (source[i] === '<') depth++
            else if (source[i] === '>') depth--
            i++
          }
        }
        if (i < n && source[i] === '(') {
          let depth = 1
          i++
          while (i < n && depth > 0) {
            if (source[i] === '(') depth++
            else if (source[i] === ')') depth--
            i++
          }
        }
        while (i < n && (source[i] === ' ' || source[i] === '\t')) i++
        if (i < n && source[i] === '\r') i++
        if (i < n && source[i] === '\n') i++
        continue
      } else {
        result += source.slice(wsStart, i)
      }
    } else {
      result += source.slice(wsStart, i)
      if (i < n) {
        result += source[i]
        i++
      }
    }
  }
  return result
}

const JS_HANDLE_MOVE_PATCH = `
const _movedHandles = new WeakSet();
const _origDispose = JSHandle.prototype[disposeSymbol];
if (_origDispose) {
  JSHandle.prototype[disposeSymbol] = function() {
    if (_movedHandles.has(this)) {
      _movedHandles.delete(this);
      return;
    }
    return _origDispose.call(this);
  };
}
const _origAsyncDispose = JSHandle.prototype[asyncDisposeSymbol];
if (_origAsyncDispose) {
  JSHandle.prototype[asyncDisposeSymbol] = function() {
    if (_movedHandles.has(this)) {
      _movedHandles.delete(this);
      return;
    }
    return _origAsyncDispose.call(this);
  };
}
JSHandle.prototype.move = function() {
  _movedHandles.add(this);
  return this;
};
`

export async function load(url, context, nextLoad) {
  if (url.endsWith('.d.ts')) {
    return {
      format: 'module',
      source: 'export default {};\n',
      shortCircuit: true,
    }
  }

  if (url.endsWith('.ts') || url.endsWith('.mts')) {
    const filePath = fileURLToPath(url)
    const source = readFileSync(filePath, 'utf8')
    let cleanSource = robustStrip(source)
    if (filePath.includes('puppeteer-core/src/api/JSHandle.ts')) {
      cleanSource += '\n' + JS_HANDLE_MOVE_PATCH + '\n'
    }
    if (filePath.includes('rxjs') && filePath.endsWith('index.ts')) {
      const typeSymbols = [
        'BasicGroupByOptions',
        'GroupByOptionsWithElement',
        'ConnectConfig',
        'RepeatConfig',
        'RetryConfig',
        'ShareConfig',
        'ShareReplayConfig',
        'TapObserver',
        'ThrottleConfig',
        'TimeoutConfig',
        'TimeoutInfo',
        'GlobalConfig',
        'Operator',
        'GroupedObservable',
      ]
      for (const s of typeSymbols) {
        cleanSource = cleanSource.replace(
          new RegExp('\\b' + s + '\\b', 'g'),
          'type ' + s,
        )
      }
    }
    const transform = getTransformSync()
    const { code } = transform(cleanSource, {
      loader: 'ts',
      format: 'esm',
      target: 'es2022',
      sourcefile: filePath,
    })
    return {
      format: 'module',
      source: code,
      shortCircuit: true,
    }
  }

  return nextLoad(url, context)
}

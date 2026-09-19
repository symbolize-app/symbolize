import { register } from 'node:module'
import { fileURLToPath } from 'node:url'
import { dirname, resolve as pathResolve } from 'node:path'

import { existsSync } from 'node:fs'

const currentDir = dirname(fileURLToPath(import.meta.url))
const repoRoot = process.env.DEVENV_ROOT ?? pathResolve(currentDir, '..')

function validPath(p) {
  return typeof p === 'string' && existsSync(p)
}

if (
  !validPath(process.env.BETTER_SQLITE3_BINDING) ||
  !validPath(process.env.ESBUILD_BINARY_PATH) ||
  !validPath(process.env.ESBUILD_MAIN_PATH) ||
  !validPath(process.env.PUPPETEER_INJECTED_PATH)
) {
  try {
    const { execFileSync } = await import('node:child_process')
    const mode = process.env.TASK_GEN_MODE || 'release'
    const out = execFileSync(
      'buck2',
      [
        'build',
        '-m',
        mode,
        '--show-output',
        'vendor//better-sqlite3-11.1.2:better-sqlite3',
        'vendor//esbuild-0.19.5:esbuild',
        'vendor//esbuild-0.19.5:main.js',
        'vendor//puppeteer-25.9.0:injected',
      ],
      {
        cwd: repoRoot,
        encoding: 'utf8',
        stdio: ['pipe', 'pipe', 'inherit'],
      },
    )
    for (const line of out.trim().split('\n')) {
      const parts = line.trim().split(/\s+/)
      if (parts.length >= 2) {
        const [target, relPath] = parts
        const fullPath = pathResolve(repoRoot, relPath)
        if (
          target.includes('better-sqlite3') &&
          !validPath(process.env.BETTER_SQLITE3_BINDING)
        ) {
          process.env.BETTER_SQLITE3_BINDING = fullPath
        } else if (
          target.includes('esbuild-0.19.5:esbuild') &&
          !validPath(process.env.ESBUILD_BINARY_PATH)
        ) {
          process.env.ESBUILD_BINARY_PATH = fullPath
        } else if (
          target.includes('esbuild-0.19.5:main.js') &&
          !validPath(process.env.ESBUILD_MAIN_PATH)
        ) {
          process.env.ESBUILD_MAIN_PATH = fullPath
        } else if (
          target.includes('puppeteer-25.9.0:injected') &&
          !validPath(process.env.PUPPETEER_INJECTED_PATH)
        ) {
          process.env.PUPPETEER_INJECTED_PATH = fullPath
        }
      }
    }
  } catch {
    // If buck2 is unavailable, let runtime errors surface when missing artifacts are accessed
  }
}

register('./hooks.mjs', import.meta.url)

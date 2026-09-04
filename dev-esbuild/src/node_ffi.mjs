import { execFile as runFile } from 'node:child_process'
import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs'
import { mkdir as makeDirectory, rm } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import {
  resolve as resolvePath,
  relative as relativePath,
} from 'node:path'
import { Result$Error, Result$Ok, List } from './gleam.mjs'

export function argv() {
  return List.fromArray(process.argv.slice(2))
}

export function resolve(path) {
  return resolvePath(path)
}

export function relative(from, to) {
  return relativePath(from, to)
}

export function now_milliseconds() {
  return Date.now()
}

export function remove(path, callback) {
  rm(path, { force: true, recursive: true }).then(
    () => callback(Result$Ok(undefined)),
    (error) => callback(Result$Error(String(error))),
  )
}

export function mkdir(path, callback) {
  makeDirectory(path, { recursive: true }).then(
    () => callback(Result$Ok(undefined)),
    (error) => callback(Result$Error(String(error))),
  )
}

export function temporary_directory(prefix) {
  return mkdtempSync(`${tmpdir()}/${prefix}`)
}

export function write_file(path, contents) {
  writeFileSync(path, Buffer.from(contents.rawBuffer))
}

export function read_file(path) {
  return readFileSync(path, 'utf8')
}

export function new_environment() {
  return {}
}

export function set_environment(environment, name, value) {
  environment[name] = value
}

export function exec_file(command, args, environment, callback) {
  runFile(
    command,
    args.toArray(),
    {
      env: {
        ...process.env,
        ...environment,
      },
    },
    (error) =>
      callback(error ? Result$Error(error.message) : Result$Ok(undefined)),
  )
}

export function set_failure() {
  process.exitCode = 1
}

import {
  Option$None$const,
  Option$Some,
} from '../gleam_stdlib/gleam/option.mjs'
import { Result$Error, Result$Ok, toBitArray } from './gleam.mjs'
import DatabaseConstructor from 'better-sqlite3'

export function open(path, readonly) {
  return new DatabaseConstructor(path, {
    fileMustExist: path !== ':memory:',
    readonly,
  })
}

export function safe_integers(database) {
  database.defaultSafeIntegers()
}

export function execute(database, sql) {
  database.exec(sql)
}

export function pragma(database, sql) {
  database.pragma(sql)
}

export function prepare(database, sql) {
  return database.prepare(sql)
}

export function new_parameters() {
  return {}
}

export function set_text_parameter(parameters, name, value) {
  parameters[name] = value
}

export function set_integer_parameter(parameters, name, value) {
  parameters[name] = value
}

export function set_blob_parameter(parameters, name, value) {
  parameters[name] = Buffer.from(value.rawBuffer)
}

export function run(statement, parameters) {
  const result = statement.run(parameters)
  return [Number(result.lastInsertRowid), Number(result.changes)]
}

export function get_count(statement) {
  return Number(statement.get().row_count)
}

export function get_blob_by_text(statement, column, parameter) {
  return blob_result(statement.get(parameter), column)
}

export function get_blob_by_blob(statement, column, parameter) {
  return blob_result(
    statement.get(Buffer.from(parameter.rawBuffer)),
    column,
  )
}

export function call_and_catch(action) {
  try {
    return Result$Ok(action())
  } catch (error) {
    return Result$Error(String(error))
  }
}

function blob_result(row, column) {
  return row === undefined || row === null ?
      Option$None$const
    : Option$Some(toBitArray([row[column]]))
}

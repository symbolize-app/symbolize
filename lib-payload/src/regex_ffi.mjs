export function new_regex(source, flags) {
  return new RegExp(source, flags)
}

export function exec_regex(regex, value) {
  return regex.exec(value) !== null
}

export function regex_text(regex) {
  return regex.toString()
}

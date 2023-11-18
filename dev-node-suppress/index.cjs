/* eslint-env node */
/* eslint-disable @typescript-eslint/unbound-method */
'use strict'

const originalEmitWarning = process.emitWarning

process.emitWarning = (warning, ...args) => {
  if (args[0] === 'ExperimentalWarning') {
    return
  }

  if (
    args[0] &&
    typeof args[0] === 'object' &&
    args[0].type === 'ExperimentalWarning'
  ) {
    return
  }

  return originalEmitWarning(
    warning,
    .../** @type any */ (args)
  )
}

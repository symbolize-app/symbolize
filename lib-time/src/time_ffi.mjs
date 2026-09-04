export function new_system_time() {
  return {}
}

export function system_performance_now(_time) {
  return globalThis.performance.now()
}

export function system_set_timeout(_time, callback, milliseconds) {
  globalThis.setTimeout(callback, milliseconds)
}

export function date_from_iso(iso) {
  return new globalThis.Date(iso)
}

export function date_get_time(date) {
  return date.getTime()
}

export function date_to_iso(date) {
  return date.toISOString()
}

export function date_add(date, milliseconds) {
  return new globalThis.Date(date.getTime() + milliseconds)
}

export function new_abort_controller() {
  return new globalThis.AbortController()
}

export function abort_controller_signal(controller) {
  return controller.signal
}

export function abort_controller_abort(controller) {
  controller.abort()
}

export function signal_aborted(signal) {
  return signal.aborted
}

export function system_delay_or_abort(
  _time,
  signal,
  milliseconds,
  on_timer,
  on_abort,
) {
  delay_or_abort(
    signal,
    milliseconds,
    on_timer,
    on_abort,
    (callback, delay) => globalThis.setTimeout(callback, delay),
    (timer) => globalThis.clearTimeout(timer),
  )
}

export function scheduled_delay_or_abort(
  set_timeout,
  signal,
  milliseconds,
  on_timer,
  on_abort,
) {
  delay_or_abort(
    signal,
    milliseconds,
    on_timer,
    on_abort,
    (callback, delay) => {
      set_timeout(callback, delay)
      return undefined
    },
    (_timer) => {},
  )
}

function delay_or_abort(
  signal,
  milliseconds,
  on_timer,
  on_abort,
  set_timer,
  clear_timer,
) {
  let timer
  const abort = () => {
    clear_timer(timer)
    signal.removeEventListener('abort', abort)
    on_abort()
  }
  const timer_callback = () => {
    if (signal.aborted) return
    signal.removeEventListener('abort', abort)
    on_timer()
  }
  signal.addEventListener('abort', abort, { once: true })
  if (signal.aborted) abort()
  else timer = set_timer(timer_callback, milliseconds)
}

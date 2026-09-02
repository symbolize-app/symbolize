export function navigator() {
  return globalThis.navigator
}

export function listen_for_controller_change(navigator, onChange) {
  navigator.serviceWorker.addEventListener('controllerchange', onChange)
}

export function register(navigator, path, scope, onRegistered, onError) {
  navigator.serviceWorker
    .register(path, {
      scope,
      updateViaCache: 'none',
    })
    .then(onRegistered)
    .catch((error) => onError(String(error)))
}

export function listen_for_update(registration, onUpdate) {
  registration.addEventListener('updatefound', onUpdate)
}

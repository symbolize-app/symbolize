export function listenForMessage(): void {
  navigator.serviceWorker.addEventListener('message', (event) => {
    if (event.data === 'reload') {
      // eslint-disable-next-line no-console
      console.log('reload')
      location.reload()
    }
  })
  navigator.serviceWorker.startMessages()
}

export function listenForKeyboardShortcut(): void {
  const isMac = /^mac/i.test(navigator.platform)

  window.addEventListener('keydown', (event) => {
    if (event.key === 's' && (isMac ? event.metaKey : event.ctrlKey)) {
      void update()
      event.preventDefault()
    }
  })
}

async function update(): Promise<void> {
  // eslint-disable-next-line no-console
  console.log('update')
  const registration = await navigator.serviceWorker.getRegistration()
  if (!registration) {
    throw new Error('Missing service worker registration')
  }
  await registration.update()
}

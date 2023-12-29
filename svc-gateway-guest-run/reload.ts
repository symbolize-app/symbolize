export function listenForMessage() {
  navigator.serviceWorker.addEventListener('message', (event) => {
    if (event.data === 'reload') {
      console.log('reload')
      location.reload()
    }
  })
  navigator.serviceWorker.startMessages()
}

export function listenForKeyboardShortcut() {
  const isMac = /^mac/i.test(navigator.platform)

  window.addEventListener('keydown', (event) => {
    if (event.key === 's' && (isMac ? event.metaKey : event.ctrlKey)) {
      void update()
      event.preventDefault()
    }
  })
}

async function update() {
  console.log('update')
  const registration = await navigator.serviceWorker.getRegistration()
  await registration!.update()
}

export type Context = {
  worker: Worker
}

export function initContext(): Context {
  const worker = new Worker(
    '/.code/svc-gateway-guest-run/dedicatedWorker.ts.mjs',
    { type: 'module' }
  )
  worker.addEventListener('message', (event) => {
    console.log('worker message', event)
  })
  worker.postMessage('ping')
  console.log('worker', worker)
  return { worker }
}

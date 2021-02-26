/* eslint-env node */

import snowpack from 'snowpack'

async function run() {
  /** @type {snowpack.SSRLoader} */
  let runtime
  const config = await snowpack.loadConfiguration({
    routes: [
      {
        match: 'routes',
        src: '/',
        dest: '/',
      },
      {
        match: 'routes',
        src: '/(?!_snowpack/).*',
        dest: async (req, res) => {
          // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
          const serve = (
            await runtime.importModule('/js/api/serve.js')
          ).exports
          // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
          serve.handleRequest(req, res)
        },
      },
    ],
  })
  const server = await snowpack.startServer({
    config,
    lockfile: null,
  })
  runtime = server.getServerRuntime({
    invalidateOnChange: true,
  })
}

run().catch((error) => {
  console.error(error)
  process.abort()
})

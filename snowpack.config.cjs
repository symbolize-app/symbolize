/* eslint-env commonjs */

/** @type {import("snowpack").SnowpackUserConfig } */
const config = {
  alias: {
    '@fe/api': './api',
    '@fe/core': './core',
    '@fe/db': './db',
    '@fe/ui': './ui',
    '@tiny': './tiny',
  },
  buildOptions: {},
  devOptions: {},
  mount: {
    api: { url: '/js/api' },
    core: { url: '/js/core' },
    db: { url: '/js/db' },
    public: { url: '/', static: true },
    tiny: { url: '/js/tiny' },
    ui: { url: '/js/ui' },
  },
  optimize: {
    bundle: false,
    minify: true,
    target: 'es2020',
  },
  packageOptions: {
    source: "local",
    external: ["http", "url"]
  },
  plugins: [],
}

module.exports = config

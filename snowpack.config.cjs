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
    public: { url: '/', static: true },
    ui: { url: '/js/ui' },
    "tiny/ui": { url: '/js/tiny/ui' },
  },
  optimize: {
    bundle: false,
    minify: true,
    target: 'es2018',
  },
  packageOptions: {},
  plugins: [],
}

module.exports = config

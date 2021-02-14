/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    public: { url: '/', static: true },
    ui: { url: '/js/ui' },
    tiny: { url: '/js/ui' },
  },
  plugins: [],
  packageOptions: {},
  devOptions: {},
  buildOptions: {},
  optimize: {
    bundle: false,
    minify: true,
    target: 'es2018',
  },
}

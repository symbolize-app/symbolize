declare module 'prettier/plugins/postcss.mjs' {
  export = unknown
}

declare module 'prettier/standalone.mjs' {
  export function format(
    code: string,
    options: {
      readonly parser: string
      readonly plugins: readonly unknown[]
    },
  ): Promise<string>
}

/* eslint-disable @typescript-eslint/naming-convention */
/* eslint-disable functional/type-declaration-immutability */

interface ImportMeta {
  readonly env: {
    readonly NODE_ENV: string
  }
}

declare module '*.css' {
  const text: string
  // eslint-disable-next-line import/no-default-export -- most convenient way to consume
  export default text
}

declare module '*.html' {
  const text: string
  // eslint-disable-next-line import/no-default-export -- most convenient way to consume
  export default text
}

declare module '*.txt' {
  const text: string
  // eslint-disable-next-line import/no-default-export -- most convenient way to consume
  export default text
}

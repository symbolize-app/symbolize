import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveyHtmlAttrs from '@/htmlAttrs.ts'

export const html = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      const namespace = null
      return (attrs: object) =>
        new conveyElement.ElementFragment(namespace, tag, attrs)
    },
  },
) as unknown as {
  readonly [Key in keyof conveyHtmlAttrs.AttrsTagNameMap<unknown> &
    keyof HTMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveyHtmlAttrs.AttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}

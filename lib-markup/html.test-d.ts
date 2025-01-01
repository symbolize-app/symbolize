import type * as markupElementAttrTest from '@/elementAttr.test-d.ts'
import type * as markupHtml from '@/html.ts'

export const url = import.meta.url

export const tests = {
  ['button button attrs'](): void {
    const tag = 'button'
    let actual: ActualForType<typeof tag, 'button'> = null as never
    let expected: ExpectedForType<
      typeof tag,
      'button',
      | 'form'
      | 'formAction'
      | 'formEnctype'
      | 'formMethod'
      | 'formNoValidate'
      | 'formTarget'
      | 'name'
      | 'value'
    > = null as never
    actual = expected
    expected = actual
  },

  ['button reset attrs'](): void {
    const tag = 'button'
    let actual: ActualForType<typeof tag, 'reset'> = null as never
    let expected: ExpectedForType<
      typeof tag,
      'reset',
      | 'formAction'
      | 'formEnctype'
      | 'formMethod'
      | 'formNoValidate'
      | 'formTarget'
      | 'name'
      | 'value'
    > = null as never
    actual = expected
    expected = actual
  },

  ['button submit attrs'](): void {
    const tag = 'button'
    let actual: ActualForType<typeof tag, 'submit'> = null as never
    let expected: ExpectedForType<typeof tag, 'submit', never> =
      null as never
    actual = expected
    expected = actual
  },

  ['div attrs'](): void {
    const tag = 'div'
    let actual: Actual<typeof tag> = null as never
    let expected: Expected<typeof tag> = null as never
    actual = expected
    expected = actual
  },

  ['input checkbox attrs'](): void {
    const tag = 'input'
    let actual: ActualForType<typeof tag, 'checkbox'> = null as never
    let expected: ExpectedForType<
      typeof tag,
      'checkbox',
      | 'accept'
      | 'alt'
      | 'autocomplete'
      | 'capture'
      | 'dirName'
      | 'files'
      | 'formAction'
      | 'formEnctype'
      | 'formMethod'
      | 'formNoValidate'
      | 'formTarget'
      | 'height'
      | 'indeterminate'
      | 'list'
      | 'max'
      | 'maxLength'
      | 'min'
      | 'minLength'
      | 'multiple'
      | 'pattern'
      | 'placeholder'
      | 'readOnly'
      | 'size'
      | 'src'
      | 'step'
      | 'useMap'
      | 'width'
    > = null as never
    actual = expected
    expected = actual
  },

  ['input text attrs'](): void {
    const tag = 'input'
    let actual: ActualForType<typeof tag, 'text'> = null as never
    let expected: ExpectedForType<
      typeof tag,
      'text',
      | 'accept'
      | 'alt'
      | 'capture'
      | 'checked'
      | 'dirName'
      | 'files'
      | 'formAction'
      | 'formEnctype'
      | 'formMethod'
      | 'formNoValidate'
      | 'formTarget'
      | 'height'
      | 'indeterminate'
      | 'max'
      | 'min'
      | 'multiple'
      | 'src'
      | 'step'
      | 'useMap'
      | 'width'
    > = null as never
    actual = expected
    expected = actual
  },
}

type Actual<Tag extends keyof typeof markupHtml.html> = Readonly<
  Required<Parameters<(typeof markupHtml.html)[Tag]>[0]>
>

type ActualForType<
  Tag extends keyof typeof markupHtml.html,
  Type extends string,
> = Actual<Tag> & { readonly type: Type }

type Expected<Tag extends keyof HTMLElementTagNameMap> =
  markupElementAttrTest.TestAttrs<unknown, HTMLElementTagNameMap[Tag]>

type ExpectedForType<
  Tag extends keyof HTMLElementTagNameMap,
  Type extends string,
  OmitKeys extends string,
> = Omit<Expected<Tag>, OmitKeys | 'type'> & { readonly type: Type }

import * as button from '@fe/ui/button.ts'
import * as widgetTest from '@tiny/ui/widget.t.ts'
import * as widget from '@tiny/ui/widget.ts'
import chai from 'chai'

export const url = import.meta.url

export const tests = {
  ['button text'](): void {
    const document = window.document.implementation.createHTMLDocument(
      ''
    )
    const ctx = widget.initContext(document)
    const buttonElement = widgetTest.collectOne(
      button.custom(ctx, {})
    )
    chai.expect(buttonElement).to.have.text('OK')
  },
}

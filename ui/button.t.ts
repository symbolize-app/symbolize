import * as button from '@fe/ui/button.ts'
import * as widgetTest from '@tiny/ui/widget.t.ts'
import chai from 'chai'

export const url = import.meta.url

export const tests = {
  ['button text'](): void {
    const buttonElement = widgetTest.collectOne(
      button.custom({})
    )
    chai.expect(buttonElement).to.have.text('OK')
  },
}

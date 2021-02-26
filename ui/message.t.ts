import * as message from '@fe/ui/message.ts'
import * as chai from 'chai'

export const url = import.meta.url

export const tests = {
  ['hi message'](): void {
    chai.expect(message.hi).to.equal('Hello')
  },
}

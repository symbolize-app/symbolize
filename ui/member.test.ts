import * as uiMember from '@fe/ui/member.ts'
import * as submitTest from '@tiny/api/submit.test.ts'
import type * as submit from '@tiny/api/submit.ts'
import * as test from '@tiny/test/index.ts'
import * as widgetTest from '@tiny/ui/widget.test.ts'
import type * as widget from '@tiny/ui/widget.ts'
import type * as errorModule from '@tiny/util/error.ts'

export const url = import.meta.url

export const tests = {
  ['member create, no error']: widgetTest.withTempDocument(
    async (baseContext: test.Context & widget.Context) => {
      const submit = test.mock<
        () => Promise<submit.Response>
      >([
        () =>
          Promise.resolve(
            submitTest.mockResponse({
              json: () => Promise.resolve({ id: 'ABCDEF' }),
            })
          ),
      ])
      const ctx: test.Context &
        widget.Context &
        errorModule.Context &
        submit.Context = {
        ...baseContext,
        submit,
      }
      ctx.document.body.content = [uiMember.custom(ctx, {})]
      const form = ctx.document.body.querySelector<HTMLFormElement>(
        ':scope > form'
      )
      const submitButton = form?.querySelector<HTMLButtonElement>(
        ':scope > button'
      )
      const status = form?.querySelector<HTMLDivElement>(
        ':scope > div'
      )
      test.assertEquals(submitButton?.textContent, 'Submit')
      test.assertEquals(submitButton?.type, 'submit')
      submitButton?.click()
      await ctx.clock.tickAsync(0)
      test.assertEquals(submit[test.mockHistory].length, 1)
      test.assertEquals(
        status?.textContent,
        'Member created {"id":"ABCDEF"}'
      )
      test.assertDeepEquals(submit[test.mockHistory], [
        [
          {
            path: '/api/member/create',
            method: 'POST',
            body:
              '{"requestId":"94194353ecc2a1448503e12775b8a20dc956a9ca26ef10f2fa930be7931bfa74","email":"a@b.com","handle":"aaa"}',
          },
        ],
      ])
    }
  ),
  ['member create, uniqueness error']: widgetTest.withTempDocument(
    async (baseContext: test.Context & widget.Context) => {
      const submit = test.mock<
        () => Promise<submit.Response>
      >([
        () =>
          Promise.resolve(
            submitTest.mockResponse({
              status: 409,
              json: () =>
                Promise.resolve({ conflict: 'email' }),
            })
          ),
      ])
      const ctx: test.Context &
        widget.Context &
        errorModule.Context &
        submit.Context = {
        ...baseContext,
        submit,
      }
      ctx.document.body.content = [uiMember.custom(ctx, {})]
      const form = ctx.document.body.querySelector<HTMLFormElement>(
        ':scope > form'
      )
      const submitButton = form?.querySelector<HTMLButtonElement>(
        ':scope > button'
      )
      const status = form?.querySelector<HTMLDivElement>(
        ':scope > div'
      )
      test.assertEquals(submitButton?.textContent, 'Submit')
      test.assertEquals(submitButton?.type, 'submit')
      submitButton?.click()
      await ctx.clock.tickAsync(0)
      test.assertEquals(submit[test.mockHistory].length, 1)
      test.assertEquals(
        status?.textContent,
        'Unique constraint error email'
      )
      test.assertDeepEquals(submit[test.mockHistory], [
        [
          {
            path: '/api/member/create',
            method: 'POST',
            body:
              '{"requestId":"94194353ecc2a1448503e12775b8a20dc956a9ca26ef10f2fa930be7931bfa74","email":"a@b.com","handle":"aaa"}',
          },
        ],
      ])
    }
  ),
}

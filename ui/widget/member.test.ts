import * as appWidgetMember from '@fe/ui/widget/member.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as test from '@tiny/test/index.ts'
import * as submitTest from '@tiny/ui/submit.test.ts'
import type * as submit from '@tiny/ui/submit.ts'
import * as widgetTest from '@tiny/ui/widget.test.ts'
import type * as widget from '@tiny/ui/widget.ts'

export const url = import.meta.url

export const tests = {
  ['member create, no error']: widgetTest.withTempDocument(
    async (baseContext: test.Context & widget.Context) => {
      const id =
        'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
      const submit = test.mock<
        () => Promise<submit.Response>
      >([
        () =>
          Promise.resolve(
            submitTest.mockResponse({
              json: () => Promise.resolve({ id }),
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
      ctx.document.body.content = [
        appWidgetMember.custom(ctx, {}),
      ]
      const form = ctx.document.body.querySelector(
        ':scope > form'
      )
      test.assertInstanceOf(
        form,
        ctx.window.HTMLFormElement
      )
      const submitButton = form.querySelector(
        ':scope > button'
      )
      test.assertInstanceOf(
        submitButton,
        ctx.window.HTMLButtonElement
      )
      const status = form.querySelector(':scope > div')
      test.assertInstanceOf(
        status,
        ctx.window.HTMLDivElement
      )
      test.assertEquals(submitButton.textContent, 'Submit')
      test.assertEquals(submitButton.type, 'submit')
      submitButton.click()
      await ctx.clock.tickAsync(0)
      test.assertEquals(
        status?.textContent,
        `Member created {"id":"${id}"}`
      )
      test.assertDeepEquals(submit[test.mockHistory], [
        [
          {
            path: '/api/member/create',
            method: 'POST',
            headers: {
              'content-type': 'application/json',
            },
            body: {
              requestId:
                '94194353ecc2a1448503e12775b8a20dc956a9ca26ef10f2fa930be7931bfa74',
              email: 'a@b.com',
              handle: 'aaa',
            },
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
      ctx.document.body.content = [
        appWidgetMember.custom(ctx, {}),
      ]
      const form = ctx.document.body.querySelector(
        ':scope > form'
      )
      test.assertInstanceOf(
        form,
        ctx.window.HTMLFormElement
      )
      const submitButton = form.querySelector(
        ':scope > button'
      )
      test.assertInstanceOf(
        submitButton,
        ctx.window.HTMLButtonElement
      )
      const status = form.querySelector(':scope > div')
      test.assertInstanceOf(
        status,
        ctx.window.HTMLDivElement
      )
      test.assertEquals(submitButton.textContent, 'Submit')
      test.assertEquals(submitButton.type, 'submit')
      submitButton.click()
      await ctx.clock.tickAsync(0)
      test.assertEquals(
        status.textContent,
        'Unique constraint error email'
      )
      test.assertDeepEquals(submit[test.mockHistory], [
        [
          {
            path: '/api/member/create',
            method: 'POST',
            headers: {
              'content-type': 'application/json',
            },
            body: {
              requestId:
                '94194353ecc2a1448503e12775b8a20dc956a9ca26ef10f2fa930be7931bfa74',
              email: 'a@b.com',
              handle: 'aaa',
            },
          },
        ],
      ])
    }
  ),
}

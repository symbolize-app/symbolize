import * as appWidgetMember from '@app/ui/widget/member.ts'
import * as tinyErrorTest from '@tiny/core/error.test.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinySubmitTest from '@tiny/core/submit.test.ts'
import type * as tinySubmit from '@tiny/core/submit.ts'
import * as tinyTest from '@tiny/test/index.ts'
import * as tinyWidgetTest from '@tiny/ui/widget.test.ts'
import type * as tinyWidget from '@tiny/ui/widget.ts'

export const url = import.meta.url

export const tests = {
  ['member create, no error']:
    tinyWidgetTest.withTempDocument(
      async (
        baseContext: tinyTest.Context & tinyWidget.Context
      ) => {
        const id =
          'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
        const submit = tinyTest.mock<
          () => Promise<tinySubmit.Response>
        >([
          () =>
            Promise.resolve(
              tinySubmitTest.mockResponse({
                json: () => Promise.resolve({ id }),
              })
            ),
        ])
        const ctx: tinyTest.Context &
          tinyWidget.Context &
          tinyError.Context &
          tinySubmit.Context = {
          ...baseContext,
          submit,
          submitRetryConfig: tinyErrorTest.retryConfig,
        }
        ctx.document.body.content = [
          appWidgetMember.create(ctx, {}),
        ]
        const form =
          ctx.document.body.querySelector(':scope > form')
        tinyTest.assertInstanceOf(
          form,
          ctx.window.HTMLFormElement
        )
        const submitButton = form.querySelector(
          ':scope > button'
        )
        tinyTest.assertInstanceOf(
          submitButton,
          ctx.window.HTMLButtonElement
        )
        const status = form.querySelector(':scope > div')
        tinyTest.assertInstanceOf(
          status,
          ctx.window.HTMLDivElement
        )
        tinyTest.assertEquals(
          submitButton.textContent,
          'Submit'
        )
        tinyTest.assertEquals(submitButton.type, 'submit')
        submitButton.click()
        await ctx.clock.tickAsync(0)
        tinyTest.assertEquals(
          status?.textContent,
          `Member created {"id":"${id}"}`
        )
        tinyTest.assertDeepEquals(
          submit[tinyTest.mockHistory],
          [
            [
              {
                path: '/api/member/create',
                method: 'POST',
                headers: {
                  'content-type': 'application/json',
                },
                json: {
                  requestId:
                    '94194353ecc2a1448503e12775b8a20dc956a9ca26ef10f2fa930be7931bfa74',
                  email: 'a@b.com',
                  handle: 'aaa',
                },
              },
            ],
          ]
        )
      }
    ),
  ['member create, uniqueness error']:
    tinyWidgetTest.withTempDocument(
      async (
        baseContext: tinyTest.Context & tinyWidget.Context
      ) => {
        const submit = tinyTest.mock<
          () => Promise<tinySubmit.Response>
        >([
          () =>
            Promise.resolve(
              tinySubmitTest.mockResponse({
                status: 409,
                json: () =>
                  Promise.resolve({ conflict: 'email' }),
              })
            ),
        ])
        const ctx: tinyTest.Context &
          tinyWidget.Context &
          tinyError.Context &
          tinySubmit.Context = {
          ...baseContext,
          submit,
          submitRetryConfig: tinyErrorTest.retryConfig,
        }
        ctx.document.body.content = [
          appWidgetMember.create(ctx, {}),
        ]
        const form =
          ctx.document.body.querySelector(':scope > form')
        tinyTest.assertInstanceOf(
          form,
          ctx.window.HTMLFormElement
        )
        const submitButton = form.querySelector(
          ':scope > button'
        )
        tinyTest.assertInstanceOf(
          submitButton,
          ctx.window.HTMLButtonElement
        )
        const status = form.querySelector(':scope > div')
        tinyTest.assertInstanceOf(
          status,
          ctx.window.HTMLDivElement
        )
        tinyTest.assertEquals(
          submitButton.textContent,
          'Submit'
        )
        tinyTest.assertEquals(submitButton.type, 'submit')
        submitButton.click()
        await ctx.clock.tickAsync(0)
        tinyTest.assertEquals(
          status.textContent,
          'Unique constraint error email'
        )
        tinyTest.assertDeepEquals(
          submit[tinyTest.mockHistory],
          [
            [
              {
                path: '/api/member/create',
                method: 'POST',
                headers: {
                  'content-type': 'application/json',
                },
                json: {
                  requestId:
                    '94194353ecc2a1448503e12775b8a20dc956a9ca26ef10f2fa930be7931bfa74',
                  email: 'a@b.com',
                  handle: 'aaa',
                },
              },
            ],
          ]
        )
      }
    ),
}

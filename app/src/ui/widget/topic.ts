import * as appEndpointTopic from '@app/core/endpoint/topic.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinyRandom from '@tiny/core/random.ts'
import * as tinySubmit from '@tiny/core/submit.ts'
import * as tinyStyle from '@tiny/ui/style.ts'
import * as tinyWidget from '@tiny/ui/widget.ts'

const button = tinyWidget.html.button
const div = tinyWidget.html.div
const form = tinyWidget.html.form
const input = tinyWidget.html.input
const range = tinyWidget.range

const column = tinyStyle.build([
  {
    marginTop: '20px',
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'flex-start',
  },
])

export const list = tinyWidget.define(
  (
    ctx: tinyWidget.Context &
      tinySubmit.Context &
      tinyError.Context &
      tinyRandom.Context
  ): {
    body: tinyWidget.Widget
  } => {
    const resultsRange = range(ctx, {
      content: ['Loading...'],
    })
    const newButton = button(ctx, {
      content: ['New'],
      listen: { click: newClick },
    })
    const editRange = range(ctx, {
      content: [newButton],
    })
    const body = div(ctx, {
      styles: [column],
      content: [editRange, resultsRange],
    })
    void load()
    return {
      body,
    }

    async function load() {
      resetEditRange()
      const okResponseData = await tinySubmit.retrySubmit(
        ctx,
        'create topic',
        appEndpointTopic.list,
        {
          params: {},
        }
      )
      const { results } = okResponseData.json
      resultsRange.content = results.length
        ? results.map((result) =>
            div(ctx, {
              content: [result.title],
              listen: {
                click() {
                  editRange.content = [
                    update(ctx, {
                      topic: result,
                      listen: {
                        submit: load,
                        cancel: resetEditRange,
                      },
                    }),
                  ]
                },
              },
            })
          )
        : ['No topics']
    }

    function newClick() {
      editRange.content = [
        create(ctx, {
          listen: {
            submit: load,
            cancel: resetEditRange,
          },
        }),
      ]
    }

    function resetEditRange() {
      editRange.content = [newButton]
    }
  }
)

type CreateListeners = {
  submit?: () => void
  cancel?: () => void
}
const create = tinyWidget.define(
  (
    ctx: tinyWidget.Context &
      tinySubmit.Context &
      tinyError.Context &
      tinyRandom.Context
  ): {
    body: tinyWidget.Widget
    listen: CreateListeners
  } => {
    let listen: CreateListeners = {}
    const requestIdInput = input(ctx, {
      name: 'requestId',
      type: 'hidden',
      value: tinyRandom.requestIdHex(ctx),
    })
    const memberIdInput = input(ctx, {
      name: 'memberId',
      value:
        '7531f108efe3c66868e4d5e62f4906f7245fd37ddfa67e42b5a6ba457d76eba4',
    })
    const titleInput = input(ctx, {
      name: 'title',
      value: 'New topic',
    })
    const slugInput = input(ctx, {
      name: 'slug',
      value: 'topic',
    })
    const contentInput = input(ctx, {
      name: 'content',
      value: 'Content here',
    })
    const status = div(ctx, {})
    const body = form(ctx, {
      content: [
        requestIdInput,
        memberIdInput,
        titleInput,
        slugInput,
        contentInput,
        button(ctx, { content: ['Submit'] }),
        button(ctx, {
          type: 'button',
          content: ['Cancel'],
          listen: { click: cancel },
        }),
        status,
      ],
      listen: {
        submit: handleSubmit,
      },
    })
    return {
      body,
      set listen(value: CreateListeners) {
        listen = value
      },
    }

    function cancel() {
      listen.cancel?.()
    }

    async function handleSubmit(event: Event) {
      event.preventDefault()
      try {
        const okResponseData = await tinySubmit.retrySubmit(
          ctx,
          'create topic',
          appEndpointTopic.create,
          {
            json: {
              requestId: requestIdInput.value,
              memberId: memberIdInput.value,
              title: titleInput.value,
              slug: slugInput.value,
              content: contentInput.value,
            },
          }
        )
        status.content = [
          `Topic created ${JSON.stringify(okResponseData)}`,
        ]
        listen.submit?.()
      } catch (error) {
        if (
          error instanceof
          appEndpointTopic.create.conflictResponseJson.error
        ) {
          status.content = [
            `Unique constraint error ${error.field}`,
          ]
          return
        }
        throw error
      }
    }
  }
)

type UpdateListeners = {
  submit?: () => void
  cancel?: () => void
}

const update = tinyWidget.define(
  (
    ctx: tinyWidget.Context &
      tinySubmit.Context &
      tinyError.Context &
      tinyRandom.Context
  ): {
    body: tinyWidget.Widget
    topic: appEndpointTopic.ListResult
    listen: UpdateListeners
  } => {
    let topic: appEndpointTopic.ListResult | undefined
    let listen: UpdateListeners = {}
    const titleInput = input(ctx, {
      name: 'title',
      value: 'New topic',
    })
    const slugInput = input(ctx, {
      name: 'slug',
      value: 'topic',
    })
    const contentInput = input(ctx, {
      name: 'content',
      value: 'Content here',
    })
    const status = div(ctx, {})
    const body = form(ctx, {
      content: [
        titleInput,
        slugInput,
        contentInput,
        button(ctx, { content: ['Submit'] }),
        button(ctx, {
          type: 'button',
          content: ['Cancel'],
          listen: { click: cancel },
        }),
        status,
      ],
      listen: {
        submit: handleSubmit,
      },
    })
    return {
      body,
      set topic(value: appEndpointTopic.ListResult) {
        topic = value
        titleInput.value = value.title
        slugInput.value = value.slug
        contentInput.value = value.content
      },
      set listen(value: UpdateListeners) {
        listen = value
      },
    }

    function cancel() {
      listen.cancel?.()
    }

    async function handleSubmit(event: Event) {
      event.preventDefault()
      if (!topic) {
        return
      }
      try {
        const okResponseData = await tinySubmit.retrySubmit(
          ctx,
          'update topic',
          appEndpointTopic.update,
          {
            json: {
              id: topic.id,
              updatedOld: topic.updatedAt,
              title: titleInput.value,
              slug: slugInput.value,
              content: contentInput.value,
            },
          }
        )
        status.content = [
          `Topic created ${JSON.stringify(okResponseData)}`,
        ]
        listen.submit?.()
      } catch (error) {
        if (
          error instanceof
          appEndpointTopic.update.conflictResponseJson.error
        ) {
          status.content = [
            `Unique constraint error ${error.field}`,
          ]
          return
        }
        throw error
      }
    }
  }
)

import type * as tinyError from '@intertwine/error/error.ts'
import * as tinyRandom from '@intertwine/random/random.ts'
import * as tinyStyle from '@intertwine/style/style.ts'
import * as tinyWidget from '@intertwine/widget/widget.ts'

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

    function load() {
      resetEditRange()
      resultsRange.content = ['No topics']
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

    function handleSubmit(event: Event) {
      event.preventDefault()
      status.content = [`Topic created`]
      listen.submit?.()
    }
  }
)

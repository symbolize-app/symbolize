import type * as error from '@intertwine/lib-error'
import * as ranom from '@intertwine/lib-random'
import * as style from '@intertwine/lib-style'
import type {} from '@intertwine/lib-time'
import * as widget from '@intertwine/lib-widget'

const button = widget.html.button
const div = widget.html.div
const form = widget.html.form
const input = widget.html.input
const range = widget.range

const column = style.build([
  {
    marginTop: '20px',
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'flex-start',
  },
])

export const list = widget.define(
  (
    ctx: widget.Context & error.Context & ranom.Context
  ): {
    body: widget.Widget
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
const create = widget.define(
  (
    ctx: widget.Context & error.Context & ranom.Context
  ): {
    body: widget.Widget
    listen: CreateListeners
  } => {
    let listen: CreateListeners = {}
    const requestIdInput = input(ctx, {
      name: 'requestId',
      type: 'hidden',
      value: ranom.requestIdHex(ctx),
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

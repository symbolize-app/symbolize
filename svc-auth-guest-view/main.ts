import * as compute from '@intertwine/lib-compute'
import * as contrast from '@intertwine/lib-contrast'
import * as convey from '@intertwine/lib-convey'
import type * as random from '@intertwine/lib-random'
import type * as stream from '@intertwine/lib-stream'
import type * as time from '@intertwine/lib-time'

const fillProperty = contrast.var_<contrast.Color>()

const htmlStyle = convey.defineCustom((ctx) => {
  const html = ctx.convey.document.documentElement
  return convey.portal(html, {
    style: [contrast.boxSizing('border-box')],
  })
})

const custom = convey.defineCustom<
  unknown,
  {
    readonly title: compute.NodeOpt<string>
  }
>((ctx, attrs) => {
  const head = ctx.convey.document.head
  const countState = compute.state(0)

  return [
    convey.portal(head, {
      content: convey.html.title({
        onAdd() {
          head.querySelector('title')?.remove()
        },

        content: 'Intertwine Custom',
      }),
    }),
    convey.html.div({
      style: compute.map(
        (count) => [
          contrast.background.color(
            count % 2 ? contrast.rgb(225, 225, 225) : null,
          ),
        ],
        countState,
      ),

      onClick: compute.handler(async (_event, count) => {
        await compute.set(ctx, countState, count + 1)
      }, countState),

      content: compute.map(
        (title, count) => `${title} / ${count}`,
        attrs.title,
        countState,
      ),
    }),
    convey.svg.svg({
      style: [
        fillProperty.set([
          contrast.rgb(200, 200, 255),
          contrast.hover(contrast.rgb(0, 0, 255)),
        ]),
      ],

      height: 50,
      viewBox: [0, 0, 100, 100],
      width: 50,

      content: convey.svg.rect({
        style: [contrast.fill(fillProperty.get())],

        height: 80,
        width: 80,
        x: 10,
        y: 10,
      }),
    }),
    convey.math.math({
      style: [
        contrast.background.color(
          contrast.rgb(255, 200, [255, contrast.hover(0)]),
        ),
      ],

      display: 'block',

      content: [
        convey.math.mi({ content: 'x' }),
        convey.math.mo({ content: '+' }),
        convey.math.mi({ content: 'y' }),
      ],
    }),
  ]
})

export async function main(
  ctx: compute.Context &
    contrast.Context &
    convey.Context &
    random.Context &
    stream.ClientContext &
    time.Context,
): Promise<void> {
  const html = ctx.convey.document.documentElement
  const body = ctx.convey.document.body
  try {
    const clientSource = ctx.streamClient.connect(
      'svc-auth-guest-read',
      (data) => {
        // eslint-disable-next-line no-console
        console.log('client data', data)
      },
    )
    void clientSource.send(ctx, 'ping')

    const fragment = custom({ title: 'hello' })
    await convey
      .portal(body, { content: [htmlStyle({}), fragment] })
      .add(ctx)
  } catch (error) {
    // eslint-disable-next-line no-console
    console.error(error)
    // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
    body.append(`Loading error: ${error}`)
  } finally {
    html.classList.remove('loading')
  }
}

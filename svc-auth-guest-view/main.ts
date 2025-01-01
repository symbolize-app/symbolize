import * as compute from '@symbolize/lib-compute'
import * as markup from '@symbolize/lib-markup'
import type * as random from '@symbolize/lib-random'
import type * as stream from '@symbolize/lib-stream'
import * as styling from '@symbolize/lib-styling'
import type * as time from '@symbolize/lib-time'

const fillVar = styling.var_<styling.Color>()

const htmlStyle = markup.defineCustom((ctx) => {
  const html = ctx.markup.document.documentElement
  return markup.portal(html, {
    style: [
      styling.accent.color(
        styling.rgb(styling.pct(100), styling.pct(0), styling.pct(100)),
      ),
      styling.caret.color(
        styling.rgb(styling.pct(100), styling.pct(0), styling.pct(100)),
      ),
      styling.font.variant.ligatures(
        'common-ligatures',
        'discretionary-ligatures',
      ),
      styling.font.variant.numeric('oldstyle-nums'),
      styling.hyphen.mode('auto'),
      styling.overflow.wrap('break-word'),
      styling.punctuation.hang('first', 'last'),
      styling.text.wrap('pretty'),
    ],
  })
})

const gridColorVar = styling.var_<styling.Color>()

const bodyStyle = markup.defineCustom((ctx) => {
  const body = ctx.markup.document.body
  const transparent = styling.rgb(
    styling.pct(0),
    styling.pct(0),
    styling.pct(0),
    styling.pct(0),
  )
  return markup.portal(body, {
    style: [
      styling.overflow.x('hidden'),
      styling.position('relative'),
      styling.padding.oi(styling.rlh(2)),
      styling.size.min.h(styling.length(100, 'dvh')),

      styling.after([
        gridColorVar.set(
          styling.lightDark(
            styling.hsl(
              styling.deg(240),
              styling.pct(100),
              styling.pct(36),
              styling.pct(5),
            ),
            styling.hsl(
              styling.deg(240),
              styling.pct(100),
              styling.pct(70),
              styling.pct(20),
            ),
          ),
        ),
        styling.background.image(
          styling.gradient.linear(
            styling.deg(0),
            [gridColorVar, styling.px(1)],
            [transparent, styling.px(1)],
          ),
          styling.gradient.linear(
            styling.deg(90),
            [gridColorVar, styling.px(1)],
            [transparent, styling.px(1)],
          ),
        ),
        styling.background.size([styling.rlh(0.5), styling.rlh(0.5)]),
        styling.content(styling.stringLiteral('')),
        styling.inset.oi(styling.rlh(0)),
        styling.isolation('isolate'),
        styling.pointer.events('none'),
        styling.position('absolute'),
      ]),
    ],
  })
})

const custom = markup.defineCustom<
  unknown,
  {
    readonly title: compute.NodeOpt<string>
  }
>((ctx, attrs) => {
  const head = ctx.markup.document.head
  const countState = compute.state(0)
  const extra = styling.container.build()

  return [
    markup.portal(head, {
      content: markup.html.title({
        onAdd() {
          head.querySelector('title')?.remove()
        },

        content: 'Symbolize Custom',
      }),
    }),
    markup.html.div({
      content: [
        markup.html.h1({
          style: [
            styling.font.size(styling.rem(2)),
            styling.font.weight(700),
            styling.line.height(styling.rlh(2)),
            styling.margin.oe(styling.rlh(1)),
            styling.size.max.w(styling.length(60, 'rch')),
          ],

          content: 'The Tale of Peter Rabbit by Beatrix Potter',
        }),
        markup.html.h2({
          style: [
            styling.font.size(styling.rem(1.6)),
            styling.font.weight(500),
            styling.line.height(styling.rlh(1.5)),
            styling.margin.oe(styling.rlh(0.5)),
            styling.size.max.w(styling.length(60, 'rch')),
          ],

          content: 'Chapter 1',
        }),
        markup.html.p({
          style: [
            styling.margin.oe(styling.rlh(0.5)),
            styling.size.max.w(styling.length(60, 'rch')),
          ],

          content:
            'Once upon a time there were four little Rabbits, and their names were— Flopsy, Mopsy, Cotton-tail, and Peter.',
        }),
        markup.html.p({
          style: [
            styling.margin.oe(styling.rlh(0.5)),
            styling.size.max.w(styling.length(60, 'rch')),
          ],

          content:
            'They lived with their Mother in a sand-bank, underneath the root of a very big fir-tree.',
        }),
        markup.html.p({
          style: [styling.size.max.w(styling.length(60, 'rch'))],

          content:
            "'Now, my dears,' said old Mrs. Rabbit one morning, 'you may go into the fields or down the lane, but don't go into Mr. McGregor's garden.'",
        }),
      ],
    }),
    markup.html.hr({
      style: [
        styling.border.style.oi('none'),

        styling.border.color.oe(
          styling.lightDark(
            styling.rgb(styling.pct(0), styling.pct(0), styling.pct(0)),
            styling.rgb(
              styling.pct(100),
              styling.pct(100),
              styling.pct(100),
            ),
          ),
        ),
        styling.border.style.oe('solid'),
        styling.border.width.oe(styling.px(1)),
        styling.margin.oe(styling.sub(styling.rlh(3), styling.px(1))),
        styling.margin.os(styling.rlh(3)),
      ],
    }),
    markup.html.div({
      style: compute.map(
        (count) => [
          count % 2 ?
            styling.background.color(
              styling.rgb(
                styling.pct(80),
                styling.pct(80),
                styling.pct(80),
                styling.c(
                  styling.pct(0),
                  styling.support.match(
                    styling.support.and(
                      styling.support.code(styling.size.oi(styling.px(2))),
                      styling.support.not(
                        styling.support.code(
                          styling.size.oi(styling.px(-2)),
                        ),
                      ),
                    ),
                    styling.pct(100),
                  ),
                  styling.support.match(
                    styling.support.code(styling.size.oi(styling.px(-2))),
                    styling.pct(50),
                  ),
                ),
              ),
            )
          : null,
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
    markup.svg.svg({
      style: [
        fillVar.set(
          styling.c(
            styling.rgb(
              styling.mul(styling.pct(30), 2),
              styling.div(styling.pct(120), 2),
              styling.max(styling.pct(0), styling.pct(100)),
            ),
            styling.select.match(
              styling.select.hover(),
              styling.rgb(
                styling.pct(0),
                styling.pct(0),
                styling.pct(100),
              ),
            ),
          ),
        ),
      ],

      height: 50,
      viewBox: [0, 0, 100, 100],
      width: 50,

      content: markup.svg.rect({
        style: [styling.fill(fillVar)],

        height: 80,
        width: 80,
        x: 10,
        y: 10,
      }),
    }),
    markup.math.math({
      style: [
        styling.background.color(
          styling.rgb(
            styling.c(
              styling.pct(0),
              styling.media.match(
                'all',
                styling.media.min.w(styling.px(400)),
                styling.pct(100),
              ),
            ),
            styling.pct(75),
            styling.c(
              styling.pct(100),
              styling.select.match(
                styling.select.or(
                  styling.select.empty(),
                  styling.select.and(
                    styling.select.not(styling.select.disabled()),
                    styling.select.hover(),
                    markup.select.attr({ display: 'block', nonce: 'x' }),
                    markup.select.type('math', 'svg'),
                  ),
                ),
                styling.pct(0),
              ),
            ),
          ),
        ),
      ],

      display: 'block',
      nonce: 'x',

      content: [
        markup.math.mi({ content: 'x' }),
        markup.math.mo({ content: '+' }),
        markup.math.mi({ content: 'y' }),
      ],
    }),
    markup.html.div({
      style: [
        styling.container.name(extra),
        styling.container.type('inline-size'),
      ],

      content: [
        markup.html.input({
          checked: true,
          type: 'checkbox',
        }),
        markup.html.input({
          style: [
            styling.background.color(
              styling.container.match(
                extra,
                styling.container.min.i(styling.rlh(30)),
                styling.rgb(
                  styling.pct(100),
                  styling.pct(0),
                  styling.pct(0),
                  styling.pct(5),
                ),
              ),
            ),
          ],

          type: 'text',
          value: 'abc',
        }),
      ],
    }),
  ]
})

export async function main(
  ctx: compute.Context &
    markup.Context &
    random.Context &
    stream.WorkerClientContext &
    styling.Context &
    time.Context,
): Promise<void> {
  const html = ctx.markup.document.documentElement
  const body = ctx.markup.document.body
  try {
    const clientSource = ctx.streamClient.connect(
      'svc-auth-guest-read',
      (data) => {
        // eslint-disable-next-line no-console
        console.log('client data', data)
      },
    )
    void clientSource.send(ctx, 'ping')

    await markup
      .portal(body, {
        content: [
          htmlStyle({}),
          bodyStyle({}),
          custom({ title: 'st ffb hello' }),
        ],
      })
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

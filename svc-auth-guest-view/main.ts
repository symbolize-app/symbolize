import * as compute from '@symbolize/lib-compute'
import * as contrast from '@symbolize/lib-contrast'
import * as hypertext from '@symbolize/lib-hypertext'
import type * as random from '@symbolize/lib-random'
import type * as stream from '@symbolize/lib-stream'
import type * as time from '@symbolize/lib-time'

const fillVar = contrast.var_<contrast.Color>()

const htmlStyle = hypertext.defineCustom((ctx) => {
  const html = ctx.hypertext.document.documentElement
  return hypertext.portal(html, {
    style: [
      contrast.accent.color(
        contrast.rgb(
          contrast.pct(100),
          contrast.pct(0),
          contrast.pct(100),
        ),
      ),
      contrast.caret.color(
        contrast.rgb(
          contrast.pct(100),
          contrast.pct(0),
          contrast.pct(100),
        ),
      ),
      contrast.font.variant.ligatures(
        'common-ligatures',
        'discretionary-ligatures',
      ),
      contrast.font.variant.numeric('oldstyle-nums'),
      contrast.hyphen.mode('auto'),
      contrast.overflow.wrap('break-word'),
      contrast.punctuation.hang('first', 'last'),
      contrast.text.wrap('pretty'),
    ],
  })
})

const gridColorVar = contrast.var_<contrast.Color>()

const bodyStyle = hypertext.defineCustom((ctx) => {
  const body = ctx.hypertext.document.body
  const transparent = contrast.rgb(
    contrast.pct(0),
    contrast.pct(0),
    contrast.pct(0),
    contrast.pct(0),
  )
  return hypertext.portal(body, {
    style: [
      contrast.overflow.x('hidden'),
      contrast.position('relative'),
      contrast.padding.oi(contrast.rlh(2)),
      contrast.size.min.h(contrast.length(100, 'dvh')),

      contrast.after([
        gridColorVar.set(
          contrast.lightDark(
            contrast.hsl(
              contrast.deg(240),
              contrast.pct(100),
              contrast.pct(36),
              contrast.pct(5),
            ),
            contrast.hsl(
              contrast.deg(240),
              contrast.pct(100),
              contrast.pct(70),
              contrast.pct(20),
            ),
          ),
        ),
        contrast.background.image(
          contrast.gradient.linear(
            contrast.deg(0),
            [gridColorVar, contrast.px(1)],
            [transparent, contrast.px(1)],
          ),
          contrast.gradient.linear(
            contrast.deg(90),
            [gridColorVar, contrast.px(1)],
            [transparent, contrast.px(1)],
          ),
        ),
        contrast.background.size([contrast.rlh(0.5), contrast.rlh(0.5)]),
        contrast.content(contrast.stringLiteral('')),
        contrast.inset.oi(contrast.rlh(0)),
        contrast.isolation('isolate'),
        contrast.pointer.events('none'),
        contrast.position('absolute'),
      ]),
    ],
  })
})

const custom = hypertext.defineCustom<
  unknown,
  {
    readonly title: compute.NodeOpt<string>
  }
>((ctx, attrs) => {
  const head = ctx.hypertext.document.head
  const countState = compute.state(0)
  const extra = contrast.container.build()

  return [
    hypertext.portal(head, {
      content: hypertext.html.title({
        onAdd() {
          head.querySelector('title')?.remove()
        },

        content: 'Symbolize Custom',
      }),
    }),
    hypertext.html.div({
      content: [
        hypertext.html.h1({
          style: [
            contrast.font.size(contrast.rem(2)),
            contrast.font.weight(700),
            contrast.line.height(contrast.rlh(2)),
            contrast.margin.oe(contrast.rlh(1)),
            contrast.size.max.w(contrast.length(60, 'rch')),
          ],

          content: 'The Tale of Peter Rabbit by Beatrix Potter',
        }),
        hypertext.html.h2({
          style: [
            contrast.font.size(contrast.rem(1.6)),
            contrast.font.weight(500),
            contrast.line.height(contrast.rlh(1.5)),
            contrast.margin.oe(contrast.rlh(0.5)),
            contrast.size.max.w(contrast.length(60, 'rch')),
          ],

          content: 'Chapter 1',
        }),
        hypertext.html.p({
          style: [
            contrast.margin.oe(contrast.rlh(0.5)),
            contrast.size.max.w(contrast.length(60, 'rch')),
          ],

          content:
            'Once upon a time there were four little Rabbits, and their names were— Flopsy, Mopsy, Cotton-tail, and Peter.',
        }),
        hypertext.html.p({
          style: [
            contrast.margin.oe(contrast.rlh(0.5)),
            contrast.size.max.w(contrast.length(60, 'rch')),
          ],

          content:
            'They lived with their Mother in a sand-bank, underneath the root of a very big fir-tree.',
        }),
        hypertext.html.p({
          style: [contrast.size.max.w(contrast.length(60, 'rch'))],

          content:
            "'Now, my dears,' said old Mrs. Rabbit one morning, 'you may go into the fields or down the lane, but don't go into Mr. McGregor's garden.'",
        }),
      ],
    }),
    hypertext.html.hr({
      style: [
        contrast.border.style.oi('none'),

        contrast.border.color.oe(
          contrast.lightDark(
            contrast.rgb(
              contrast.pct(0),
              contrast.pct(0),
              contrast.pct(0),
            ),
            contrast.rgb(
              contrast.pct(100),
              contrast.pct(100),
              contrast.pct(100),
            ),
          ),
        ),
        contrast.border.style.oe('solid'),
        contrast.border.width.oe(contrast.px(1)),
        contrast.margin.oe(contrast.sub(contrast.rlh(3), contrast.px(1))),
        contrast.margin.os(contrast.rlh(3)),
      ],
    }),
    hypertext.html.div({
      style: compute.map(
        (count) => [
          count % 2 ?
            contrast.background.color(
              contrast.rgb(
                contrast.pct(80),
                contrast.pct(80),
                contrast.pct(80),
                contrast.c(
                  contrast.pct(0),
                  contrast.support.match(
                    contrast.support.and(
                      contrast.support.code(
                        contrast.size.oi(contrast.px(2)),
                      ),
                      contrast.support.not(
                        contrast.support.code(
                          contrast.size.oi(contrast.px(-2)),
                        ),
                      ),
                    ),
                    contrast.pct(100),
                  ),
                  contrast.support.match(
                    contrast.support.code(
                      contrast.size.oi(contrast.px(-2)),
                    ),
                    contrast.pct(50),
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
    hypertext.svg.svg({
      style: [
        fillVar.set(
          contrast.c(
            contrast.rgb(
              contrast.mul(contrast.pct(30), 2),
              contrast.div(contrast.pct(120), 2),
              contrast.max(contrast.pct(0), contrast.pct(100)),
            ),
            contrast.select.match(
              contrast.select.hover(),
              contrast.rgb(
                contrast.pct(0),
                contrast.pct(0),
                contrast.pct(100),
              ),
            ),
          ),
        ),
      ],

      height: 50,
      viewBox: [0, 0, 100, 100],
      width: 50,

      content: hypertext.svg.rect({
        style: [contrast.fill(fillVar)],

        height: 80,
        width: 80,
        x: 10,
        y: 10,
      }),
    }),
    hypertext.math.math({
      style: [
        contrast.background.color(
          contrast.rgb(
            contrast.c(
              contrast.pct(0),
              contrast.media.match(
                'all',
                contrast.media.min.w(contrast.px(400)),
                contrast.pct(100),
              ),
            ),
            contrast.pct(75),
            contrast.c(
              contrast.pct(100),
              contrast.select.match(
                contrast.select.or(
                  contrast.select.empty(),
                  contrast.select.and(
                    contrast.select.not(contrast.select.disabled()),
                    contrast.select.hover(),
                    hypertext.select.attr({
                      display: 'block',
                      nonce: 'x',
                    }),
                    hypertext.select.type('math', 'svg'),
                  ),
                ),
                contrast.pct(0),
              ),
            ),
          ),
        ),
      ],

      display: 'block',
      nonce: 'x',

      content: [
        hypertext.math.mi({ content: 'x' }),
        hypertext.math.mo({ content: '+' }),
        hypertext.math.mi({ content: 'y' }),
      ],
    }),
    hypertext.html.div({
      style: [
        contrast.container.name(extra),
        contrast.container.type('inline-size'),
      ],

      content: [
        hypertext.html.input({
          checked: true,
          type: 'checkbox',
        }),
        hypertext.html.input({
          style: [
            contrast.background.color(
              contrast.container.match(
                extra,
                contrast.container.min.i(contrast.rlh(30)),
                contrast.rgb(
                  contrast.pct(100),
                  contrast.pct(0),
                  contrast.pct(0),
                  contrast.pct(5),
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
    contrast.Context &
    hypertext.Context &
    random.Context &
    stream.WorkerClientContext &
    time.Context,
): Promise<void> {
  const html = ctx.hypertext.document.documentElement
  const body = ctx.hypertext.document.body
  try {
    const clientSource = ctx.streamClient.connect(
      'svc-auth-guest-read',
      (data) => {
        // eslint-disable-next-line no-console
        console.log('client data', data)
      },
    )
    void clientSource.send(ctx, 'ping')

    await hypertext
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

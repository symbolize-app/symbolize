import gleam/io
import gleam/option.{None, Some}
import lib_dataflow as dataflow
import lib_markup_attributes as markup_attributes
import lib_markup_conditional as conditional
import lib_markup_context as markup_context
import lib_markup_custom as custom
import lib_markup_data as markup_data
import lib_markup_dom as dom
import lib_markup_each as each
import lib_markup_fragment as fragment
import lib_markup_html as html
import lib_markup_math as math
import lib_markup_range as markup_range
import lib_markup_reactive as reactive
import lib_markup_style as markup_style
import lib_markup_svg as svg
import lib_markup_text as markup_text
import lib_styling as styling
import lib_styling_background as background
import lib_styling_data as styling_data
import lib_styling_typed_expr as styling_typed_expr

@external(javascript, "./async_test_ffi.mjs", "set_timeout")
fn set_timeout(callback: fn() -> Nil, milliseconds: Float) -> Nil

@external(javascript, "./each_ffi.mjs", "nan")
fn nan() -> Float

@external(javascript, "./dom_ffi.mjs", "style_layer_rule_count")
fn style_layer_rule_count(document: dom.Document, index: Int) -> Int

@external(javascript, "./dom_ffi.mjs", "call_and_catch")
fn call_and_catch(callback: fn() -> Nil) -> String

type EachItem {
  EachItem(id: Int, name: String)
}

pub fn main() {
  let document = dom.document()
  let body = dom.body(document)

  let div =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        id: dataflow.literal(Some("x")),
        aria_disabled: dataflow.literal(Some(True)),
        class_names: dataflow.literal(Some(["a", "b"])),
        nonce: dataflow.literal(Some("html-nonce")),
        role: dataflow.literal(Some("button")),
        tab_index: dataflow.literal(Some(1)),
        autofocus: dataflow.literal(Some(True)),
        content: fragment.TextInput("y"),
      ),
    )
  let mounted = dom.mount(document, body, div)
  assert dom.outer_html(dom.first(mounted))
    == "<div id=\"x\" aria-disabled=\"true\" class=\"a b\" nonce=\"html-nonce\" role=\"button\" tabindex=\"1\" autofocus=\"\">y</div>"
  dom.remove(mounted)

  let global_attrs =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        access_key: dataflow.literal(Some("x")),
        autocapitalize: dataflow.literal(Some(html.AutocapitalizeWords)),
        content_editable: dataflow.literal(Some(
          html.ContentEditablePlaintextOnly,
        )),
        dir: dataflow.literal(Some(html.HtmlDirRtl)),
        draggable: dataflow.literal(Some(True)),
        enter_key_hint: dataflow.literal(Some(html.EnterKeySearch)),
        hidden: dataflow.literal(Some(html.HiddenUntilFound)),
        inert: dataflow.literal(Some(True)),
        input_mode: dataflow.literal(Some(html.InputModeNumeric)),
        lang: dataflow.literal(Some("en")),
        slot: dataflow.literal(Some("main")),
        spellcheck: dataflow.literal(Some("false")),
        translate: dataflow.literal(Some(html.TranslateNo)),
      ),
    )
  let global_attrs_mount = dom.mount(document, body, global_attrs)
  assert dom.outer_html(dom.first(global_attrs_mount))
    == "<div accesskey=\"x\" autocapitalize=\"words\" contenteditable=\"plaintext-only\" dir=\"rtl\" draggable=\"true\" enterkeyhint=\"search\" hidden=\"until-found\" inert=\"\" inputmode=\"numeric\" lang=\"en\" slot=\"main\" spellcheck=\"false\" translate=\"no\"></div>"
  dom.remove(global_attrs_mount)

  let text_context = dataflow.dataflow()
  let text_host = dom.create_html_node(document, "div")
  dom.append_node(body, text_host)
  let text_literal_mount =
    dom.mount_reactive(
      text_context,
      document,
      text_host,
      markup_text.text(dataflow.literal("hello")),
    )
  assert dom.text_content(text_host) == "hello"
  dom.remove(text_literal_mount)

  let text_state = dataflow.state("a")
  let text_state_mount =
    dom.mount_reactive(
      text_context,
      document,
      text_host,
      markup_text.text(dataflow.mutation(text_state)),
    )
  assert dom.text_content(text_host) == "a"
  let assert Ok(Nil) =
    dataflow.txn(text_context, fn() {
      dataflow.set(text_context, text_state, "b")
    })
  assert dom.text_content(text_host) == "b"
  dom.remove(text_state_mount)
  dom.remove_node(body, text_host)

  let aria_attrs =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        aria_atomic: dataflow.literal(Some(True)),
        aria_auto_complete: dataflow.literal(Some(html.AriaAutoCompleteList)),
        aria_busy: dataflow.literal(Some(False)),
        aria_checked: dataflow.literal(Some(html.AriaMixed)),
        aria_col_count: dataflow.literal(Some("2")),
        aria_col_index: dataflow.literal(Some("1")),
        aria_col_span: dataflow.literal(Some("3")),
        aria_current: dataflow.literal(Some(html.AriaCurrentPage)),
        aria_description: dataflow.literal(Some("description")),
        aria_expanded: dataflow.literal(Some(html.AriaUndefined)),
        aria_has_popup: dataflow.literal(Some(html.AriaHasPopupMenu)),
        aria_hidden: dataflow.literal(Some(html.AriaBool(False))),
        aria_invalid: dataflow.literal(Some("grammar")),
        aria_key_shortcuts: dataflow.literal(Some("Control+S")),
        aria_label: dataflow.literal(Some("Label")),
        aria_level: dataflow.literal(Some("2")),
        aria_live: dataflow.literal(Some(html.AriaLivePolite)),
        aria_modal: dataflow.literal(Some(True)),
        aria_multi_line: dataflow.literal(Some(False)),
        aria_multi_selectable: dataflow.literal(Some(True)),
        aria_orientation: dataflow.literal(Some(html.AriaOrientationVertical)),
        aria_placeholder: dataflow.literal(Some("Placeholder")),
        aria_pos_in_set: dataflow.literal(Some("1")),
        aria_pressed: dataflow.literal(Some(html.AriaMixed)),
        aria_read_only: dataflow.literal(Some(True)),
        aria_required: dataflow.literal(Some(False)),
        aria_role_description: dataflow.literal(Some("button")),
        aria_row_count: dataflow.literal(Some("4")),
        aria_row_index: dataflow.literal(Some("1")),
        aria_row_span: dataflow.literal(Some("2")),
        aria_selected: dataflow.literal(Some(html.AriaUndefined)),
        aria_set_size: dataflow.literal(Some("4")),
        aria_sort: dataflow.literal(Some(html.AriaSortAscending)),
        aria_value_max: dataflow.literal(Some("10")),
        aria_value_min: dataflow.literal(Some("0")),
        aria_value_now: dataflow.literal(Some("5")),
        aria_value_text: dataflow.literal(Some("five")),
      ),
    )
  let aria_attrs_mount = dom.mount(document, body, aria_attrs)
  assert dom.outer_html(dom.first(aria_attrs_mount))
    == "<div aria-atomic=\"true\" aria-autocomplete=\"list\" aria-busy=\"false\" aria-checked=\"mixed\" aria-colcount=\"2\" aria-colindex=\"1\" aria-colspan=\"3\" aria-current=\"page\" aria-description=\"description\" aria-expanded=\"undefined\" aria-haspopup=\"menu\" aria-hidden=\"false\" aria-invalid=\"grammar\" aria-keyshortcuts=\"Control+S\" aria-label=\"Label\" aria-level=\"2\" aria-live=\"polite\" aria-modal=\"true\" aria-multiline=\"false\" aria-multiselectable=\"true\" aria-orientation=\"vertical\" aria-placeholder=\"Placeholder\" aria-posinset=\"1\" aria-pressed=\"mixed\" aria-readonly=\"true\" aria-required=\"false\" aria-roledescription=\"button\" aria-rowcount=\"4\" aria-rowindex=\"1\" aria-rowspan=\"2\" aria-selected=\"undefined\" aria-setsize=\"4\" aria-sort=\"ascending\" aria-valuemax=\"10\" aria-valuemin=\"0\" aria-valuenow=\"5\" aria-valuetext=\"five\"></div>"
  dom.remove(aria_attrs_mount)

  let global_context = dataflow.dataflow()
  let global_hidden = dataflow.state(Some(html.HiddenBoolean))
  let global_dir = dataflow.state(Some(html.HtmlDirLtr))
  let reactive_globals =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        dir: dataflow.mutation(global_dir),
        hidden: dataflow.mutation(global_hidden),
      ),
    )
  let reactive_globals_mount =
    dom.mount_reactive(global_context, document, body, reactive_globals)
  let reactive_globals_node = dom.first(reactive_globals_mount)
  assert dom.outer_html(reactive_globals_node)
    == "<div dir=\"ltr\" hidden=\"\"></div>"
  let assert Ok(Nil) =
    dataflow.txn(global_context, fn() {
      let assert Ok(Nil) =
        dataflow.set(global_context, global_hidden, Some(html.HiddenUntilFound))
      dataflow.set(global_context, global_dir, Some(html.HtmlDirRtl))
    })
  assert dom.outer_html(reactive_globals_node)
    == "<div dir=\"rtl\" hidden=\"until-found\"></div>"
  dom.remove(reactive_globals_mount)

  let empty_custom =
    custom.define(fn(_scoped, _attributes) { fragment.EmptyInput })
  let empty_custom_mount = dom.mount(document, body, empty_custom(#()))
  assert dom.text_content(body) == ""
  dom.remove(empty_custom_mount)

  let pure_custom =
    custom.define(fn(_scoped, content: dataflow.NodeOpt(String)) {
      fragment.ReactiveTextInput(content)
    })
  let pure_custom_mount =
    dom.mount(document, body, pure_custom(dataflow.literal("hello")))
  let pure_custom_node = dom.first(pure_custom_mount)
  assert dom.outer_html(pure_custom_node) == "hello"
  dom.remove(pure_custom_mount)

  let custom_context = dataflow.dataflow()
  let custom_title = dataflow.state("a")
  let custom_observed = dataflow.state("")
  let effect_custom =
    custom.define(fn(scoped, content: dataflow.NodeOpt(String)) {
      let scoped_context = fragment.scoped_dataflow(scoped)
      fragment.scoped_effect(
        scoped,
        fn(value) {
          let assert Ok(Nil) =
            dataflow.txn(scoped_context, fn() {
              dataflow.set(scoped_context, custom_observed, value)
            })
          Nil
        },
        dataflow.to_computation(content),
      )
      fragment.ReactiveTextInput(content)
    })
  let effect_custom_mount =
    dom.mount_reactive(
      custom_context,
      document,
      body,
      effect_custom(dataflow.mutation(custom_title)),
    )
  let effect_custom_node = dom.first(effect_custom_mount)
  assert dom.outer_html(effect_custom_node) == "a"
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(custom_observed)),
    )
    == "a"
  let assert Ok(Nil) =
    dataflow.txn(custom_context, fn() {
      dataflow.set(custom_context, custom_title, "b")
    })
  assert dom.outer_html(effect_custom_node) == "b"
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(custom_observed)),
    )
    == "b"
  dom.remove(effect_custom_mount)
  let assert Ok(Nil) =
    dataflow.txn(custom_context, fn() {
      dataflow.set(custom_context, custom_title, "c")
    })
  assert dom.outer_html(effect_custom_node) == "b"
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(custom_observed)),
    )
    == "b"

  let custom_removed = dataflow.state(False)
  let defer_custom =
    custom.define(fn(scoped, _attributes: String) {
      let scoped_context = fragment.scoped_dataflow(scoped)
      fragment.scoped_defer(scoped, fn() {
        let assert Ok(Nil) =
          dataflow.txn(scoped_context, fn() {
            dataflow.set(scoped_context, custom_removed, True)
          })
        Nil
      })
      fragment.TextInput("x")
    })
  let defer_custom_mount =
    dom.mount_reactive(custom_context, document, body, defer_custom("x"))
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(custom_removed)),
    )
    == False
  dom.remove(defer_custom_mount)
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(custom_removed)),
    )
    == True

  let event_context = dataflow.dataflow()
  let click_count = dataflow.state(0)
  let clickable =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        events: markup_attributes.Events(
          ..markup_attributes.events(),
          on_click: Some(
            markup_attributes.sync_listener(fn(_event) {
              let assert Ok(Nil) = dataflow.set(event_context, click_count, 1)
              Nil
            }),
          ),
        ),
      ),
    )
  let clickable_mount =
    dom.mount_reactive(event_context, document, body, clickable)
  dom.click(dom.first(clickable_mount))
  dom.wait(clickable_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(click_count)),
      )
      == 1
    Nil
  })

  // The source `element.ts` listener contract also accepts an async handler.
  // Use a real timer so this checks both transaction lifetime and scheduler
  // ordering at the browser boundary, rather than only an immediate callback.
  let async_event_context = dataflow.dataflow()
  let async_event_count = dataflow.state(0)
  let async_click_listener =
    dataflow.handler_async(
      fn(_event, current) {
        fn(done) {
          set_timeout(
            fn() {
              let assert Ok(Nil) =
                dataflow.set(
                  async_event_context,
                  async_event_count,
                  current + 1,
                )
              done(Ok(Nil))
            },
            0.0,
          )
        }
      },
      dataflow.mutation(async_event_count),
    )
  let async_clickable =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        events: markup_attributes.Events(
          ..markup_attributes.events(),
          on_click: Some(markup_attributes.async_listener(async_click_listener)),
        ),
      ),
    )
  let async_clickable_mount =
    dom.mount_reactive(async_event_context, document, body, async_clickable)
  dom.click(dom.first(async_clickable_mount))
  dom.wait_result(async_clickable_mount, fn(result) {
    assert result == Ok(Nil)
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(async_event_count)),
      )
      == 1
    Nil
  })
  dom.remove(async_clickable_mount)

  let queued_count = dataflow.state(0)
  let queued_fragment =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        events: markup_attributes.Events(
          ..markup_attributes.events(),
          on_click: Some(
            markup_attributes.sync_listener(fn(_event) {
              let current =
                dataflow.value(
                  dataflow.to_computation(dataflow.mutation(queued_count)),
                )
              let assert Ok(Nil) =
                dataflow.set(event_context, queued_count, current + 1)
              Nil
            }),
          ),
        ),
      ),
    )
  let queued_mount =
    dom.mount_reactive(event_context, document, body, queued_fragment)
  let queued_node = dom.first(queued_mount)
  dom.click(queued_node)
  dom.click(queued_node)
  dom.wait(queued_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(queued_count)),
      )
      == 2
    Nil
  })
  dom.remove(queued_mount)

  let on_add_context = dataflow.dataflow()
  let on_add_show = dataflow.state(False)
  let on_add_source = dataflow.state(0)
  let on_add_observed = dataflow.state(-1)
  let on_add_removed = dataflow.state(False)
  let on_add_mount =
    conditional.mount(
      on_add_context,
      document,
      body,
      dataflow.mutation(on_add_show),
      html.div(
        html.DivAttrs(
          ..html.div_attrs(),
          on_add: Some(
            fragment.sync_on_add(fn(event) {
              let fragment.OnAddEvent(scoped, element) = event
              let scoped_context = fragment.scoped_dataflow(scoped)
              assert dom.outer_html(element) == "<div>y</div>"
              fragment.scoped_effect(
                scoped,
                fn(value) {
                  let assert Ok(Nil) =
                    dataflow.txn(scoped_context, fn() {
                      dataflow.set(scoped_context, on_add_observed, value)
                    })
                  Nil
                },
                dataflow.to_computation(dataflow.mutation(on_add_source)),
              )
              fragment.scoped_defer(scoped, fn() {
                let assert Ok(Nil) =
                  dataflow.txn(scoped_context, fn() {
                    dataflow.set(scoped_context, on_add_removed, True)
                  })
                Nil
              })
            }),
          ),
          content: fragment.TextInput("y"),
        ),
      ),
      fragment.empty(),
    )
  let assert Ok(Nil) =
    dataflow.txn(on_add_context, fn() {
      dataflow.set(on_add_context, on_add_show, True)
    })
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(on_add_observed)),
    )
    == 0
  let assert Ok(Nil) =
    dataflow.txn(on_add_context, fn() {
      dataflow.set(on_add_context, on_add_source, 1)
    })
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(on_add_observed)),
    )
    == 1
  let assert Ok(Nil) =
    dataflow.txn(on_add_context, fn() {
      dataflow.set(on_add_context, on_add_show, False)
    })
  assert dataflow.value(
    dataflow.to_computation(dataflow.mutation(on_add_removed)),
  )
  let assert Ok(Nil) =
    dataflow.txn(on_add_context, fn() {
      dataflow.set(on_add_context, on_add_source, 2)
    })
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(on_add_observed)),
    )
    == 1
  conditional.remove(on_add_mount)

  let async_on_add_context = dataflow.dataflow()
  let async_on_add_started = dataflow.state(False)
  let async_on_add_connected = dataflow.state(True)
  let async_on_add_finished = dataflow.state(False)
  let async_on_add_removed = dataflow.state(False)
  let async_on_add_fragment =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        on_add: Some(
          fragment.async_on_add(fn(event) {
            let fragment.OnAddEvent(scoped, element) = event
            let scoped_context = fragment.scoped_dataflow(scoped)
            fn(done) {
              set_timeout(
                fn() {
                  assert dom.outer_html(element) == "<div>async</div>"
                  let assert Ok(Nil) =
                    dataflow.txn(scoped_context, fn() {
                      let assert Ok(Nil) =
                        dataflow.set(
                          scoped_context,
                          async_on_add_connected,
                          dom.is_connected(element),
                        )
                      dataflow.set(scoped_context, async_on_add_started, True)
                    })
                  fragment.scoped_defer(scoped, fn() {
                    let assert Ok(Nil) =
                      dataflow.txn(scoped_context, fn() {
                        dataflow.set(scoped_context, async_on_add_removed, True)
                      })
                    Nil
                  })
                  let assert Ok(Nil) =
                    dataflow.txn(scoped_context, fn() {
                      dataflow.set(scoped_context, async_on_add_finished, True)
                    })
                  done(Ok(Nil))
                },
                0.0,
              )
            }
          }),
        ),
        content: fragment.TextInput("async"),
      ),
    )
  let async_on_add_host = dom.create_html_node(document, "div")
  dom.append_node(body, async_on_add_host)
  let async_on_add_mount =
    dom.mount_reactive(
      async_on_add_context,
      document,
      async_on_add_host,
      async_on_add_fragment,
    )
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(async_on_add_started)),
    )
    == False
  dom.wait_result(async_on_add_mount, fn(result) {
    assert result == Ok(Nil)
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(async_on_add_started)),
      )
      == True
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(async_on_add_connected)),
      )
      == False
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(async_on_add_finished)),
      )
      == True
    dom.remove(async_on_add_mount)
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(async_on_add_removed)),
      )
      == True
    dom.remove_node(body, async_on_add_host)
    Nil
  })

  // A containing source element also waits for an async child add before its
  // own onAdd and before it is appended to the live tree.
  let nested_on_add_context = dataflow.dataflow()
  let nested_child_connected = dataflow.state(True)
  let nested_child_finished = dataflow.state(False)
  let nested_parent_connected = dataflow.state(True)
  let nested_parent_saw_child_finished = dataflow.state(False)
  let nested_fragment =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        on_add: Some(
          fragment.sync_on_add(fn(event) {
            let fragment.OnAddEvent(scoped, element) = event
            let scoped_context = fragment.scoped_dataflow(scoped)
            let child_finished =
              dataflow.value(
                dataflow.to_computation(dataflow.mutation(nested_child_finished)),
              )
            let assert Ok(Nil) =
              dataflow.txn(scoped_context, fn() {
                let assert Ok(Nil) =
                  dataflow.set(
                    scoped_context,
                    nested_parent_connected,
                    dom.is_connected(element),
                  )
                dataflow.set(
                  scoped_context,
                  nested_parent_saw_child_finished,
                  child_finished,
                )
              })
            Nil
          }),
        ),
        content: fragment.FragmentInput(html.div(
          html.DivAttrs(
            ..html.div_attrs(),
            on_add: Some(
              fragment.async_on_add(fn(event) {
                let fragment.OnAddEvent(scoped, element) = event
                let scoped_context = fragment.scoped_dataflow(scoped)
                fn(done) {
                  set_timeout(
                    fn() {
                      let assert Ok(Nil) =
                        dataflow.txn(scoped_context, fn() {
                          let assert Ok(Nil) =
                            dataflow.set(
                              scoped_context,
                              nested_child_connected,
                              dom.is_connected(element),
                            )
                          dataflow.set(
                            scoped_context,
                            nested_child_finished,
                            True,
                          )
                        })
                      done(Ok(Nil))
                    },
                    0.0,
                  )
                }
              }),
            ),
          ),
        )),
      ),
    )
  let nested_mount =
    dom.mount_reactive(nested_on_add_context, document, body, nested_fragment)
  assert dom.is_connected(dom.first(nested_mount)) == False
  dom.wait_result(nested_mount, fn(result) {
    assert result == Ok(Nil)
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(nested_child_connected)),
      )
      == False
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(nested_parent_connected)),
      )
      == False
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(
          nested_parent_saw_child_finished,
        )),
      )
      == True
    assert dom.is_connected(dom.first(nested_mount))
    dom.remove(nested_mount)
    Nil
  })

  // A source range adds members sequentially. A later sibling must not run
  // its onAdd or enter the live parent while an earlier member is pending.
  let range_on_add_context = dataflow.dataflow()
  let range_first_finished = dataflow.state(False)
  let range_second_saw_first_finished = dataflow.state(False)
  let range_fragment =
    fragment.range([
      fragment.FragmentInput(html.div(
        html.DivAttrs(
          ..html.div_attrs(),
          on_add: Some(
            fragment.async_on_add(fn(event) {
              let fragment.OnAddEvent(scoped, _element) = event
              let scoped_context = fragment.scoped_dataflow(scoped)
              fn(done) {
                set_timeout(
                  fn() {
                    let assert Ok(Nil) =
                      dataflow.txn(scoped_context, fn() {
                        dataflow.set(scoped_context, range_first_finished, True)
                      })
                    done(Ok(Nil))
                  },
                  0.0,
                )
              }
            }),
          ),
        ),
      )),
      fragment.FragmentInput(html.div(
        html.DivAttrs(
          ..html.div_attrs(),
          on_add: Some(
            fragment.sync_on_add(fn(event) {
              let fragment.OnAddEvent(scoped, _element) = event
              let scoped_context = fragment.scoped_dataflow(scoped)
              let first_finished =
                dataflow.value(
                  dataflow.to_computation(dataflow.mutation(
                    range_first_finished,
                  )),
                )
              let assert Ok(Nil) =
                dataflow.txn(scoped_context, fn() {
                  dataflow.set(
                    scoped_context,
                    range_second_saw_first_finished,
                    first_finished,
                  )
                })
              Nil
            }),
          ),
        ),
      )),
    ])
  let range_mount =
    dom.mount_reactive(range_on_add_context, document, body, range_fragment)
  assert dom.text_content(body) == ""
  dom.wait_result(range_mount, fn(result) {
    assert result == Ok(Nil)
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(
          range_second_saw_first_finished,
        )),
      )
      == True
    dom.remove(range_mount)
    Nil
  })

  // Removing a source fragment while its async add is pending clears the
  // fragment before the containing append can publish its node.
  let cancelled_on_add_context = dataflow.dataflow()
  let cancelled_on_add_finished = dataflow.state(False)
  let cancelled_on_add_host = dom.create_html_node(document, "div")
  dom.append_node(body, cancelled_on_add_host)
  let cancelled_on_add_fragment =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        on_add: Some(
          fragment.async_on_add(fn(event) {
            let fragment.OnAddEvent(scoped, _element) = event
            let scoped_context = fragment.scoped_dataflow(scoped)
            fn(done) {
              set_timeout(
                fn() {
                  let assert Ok(Nil) =
                    dataflow.txn(scoped_context, fn() {
                      dataflow.set(
                        scoped_context,
                        cancelled_on_add_finished,
                        True,
                      )
                    })
                  done(Ok(Nil))
                },
                0.0,
              )
            }
          }),
        ),
        content: fragment.TextInput("cancelled"),
      ),
    )
  let cancelled_on_add_mount =
    dom.mount_reactive(
      cancelled_on_add_context,
      document,
      cancelled_on_add_host,
      cancelled_on_add_fragment,
    )
  dom.remove(cancelled_on_add_mount)
  dom.wait_result(cancelled_on_add_mount, fn(result) {
    assert result == Ok(Nil)
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(cancelled_on_add_finished)),
      )
      == False
    assert dom.text_content(cancelled_on_add_host) == ""
    dom.remove_node(body, cancelled_on_add_host)
    Nil
  })

  // Source element.add runs onAdd before a normal element is appended to its
  // containing fragment. The callback still sees settled attributes/content,
  // but the element is not connected to the document yet.
  let detached_on_add_context = dataflow.dataflow()
  let detached_on_add_connected = dataflow.state(True)
  let detached_on_add_mount =
    dom.mount_reactive(
      detached_on_add_context,
      document,
      body,
      html.div(
        html.DivAttrs(
          ..html.div_attrs(),
          on_add: Some(
            fragment.sync_on_add(fn(event) {
              let fragment.OnAddEvent(_scoped, element) = event
              let assert Ok(Nil) =
                dataflow.txn(detached_on_add_context, fn() {
                  dataflow.set(
                    detached_on_add_context,
                    detached_on_add_connected,
                    dom.is_connected(element),
                  )
                })
              Nil
            }),
          ),
          content: fragment.TextInput("detached"),
        ),
      ),
    )
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(detached_on_add_connected)),
    )
    == False
  assert dom.is_connected(dom.first(detached_on_add_mount))
  dom.remove(detached_on_add_mount)

  dom.remove(clickable_mount)
  dom.click(dom.first(clickable_mount))
  dom.wait(clickable_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(click_count)),
      )
      == 1
    Nil
  })

  let common_tags =
    fragment.range([
      fragment.FragmentInput(html.h1(
        html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("h1")),
      )),
      fragment.FragmentInput(html.h2(
        html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("h2")),
      )),
      fragment.FragmentInput(html.h3(
        html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("h3")),
      )),
      fragment.FragmentInput(html.h4(
        html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("h4")),
      )),
      fragment.FragmentInput(html.h5(
        html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("h5")),
      )),
      fragment.FragmentInput(html.h6(
        html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("h6")),
      )),
      fragment.FragmentInput(html.hr(html.div_attrs())),
      fragment.FragmentInput(html.p(
        html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("p")),
      )),
      fragment.FragmentInput(html.span(
        html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("span")),
      )),
      fragment.FragmentInput(html.title(
        html.DivAttrs(..html.div_attrs(), content: fragment.TextInput("title")),
      )),
    ])
  let common_mount = dom.mount(document, body, common_tags)
  assert dom.text_content(body) == "h1h2h3h4h5h6pspantitle"
  dom.remove(common_mount)

  let button =
    html.button(html.submit_button(
      html.SubmitButtonAttrs(
        ..html.submit_button_attrs(),
        aria_disabled: dataflow.literal(Some(True)),
        form_method: dataflow.literal(Some(html.FormGet)),
        title: dataflow.literal(Some("submit")),
      ),
    ))
  let button_mount = dom.mount(document, body, button)
  assert dom.outer_html(dom.first(button_mount))
    == "<button aria-disabled=\"true\" title=\"submit\" formmethod=\"get\" type=\"submit\"></button>"
  dom.remove(button_mount)

  let reactive_button_aria_context = dataflow.dataflow()
  let reactive_button_label = dataflow.state(Some("action"))
  let reactive_button =
    html.button(html.plain_button(
      html.PlainButtonAttrs(
        ..html.plain_button_attrs(),
        aria_atomic: dataflow.literal(Some(True)),
        aria_current: dataflow.literal(Some(html.AriaCurrentPage)),
        aria_label: dataflow.mutation(reactive_button_label),
      ),
    ))
  let reactive_button_mount =
    dom.mount_reactive(
      reactive_button_aria_context,
      document,
      body,
      reactive_button,
    )
  let reactive_button_node = dom.first(reactive_button_mount)
  assert dom.outer_html(reactive_button_node)
    == "<button aria-atomic=\"true\" aria-current=\"page\" aria-label=\"action\" type=\"button\"></button>"
  let assert Ok(Nil) =
    dataflow.txn(reactive_button_aria_context, fn() {
      dataflow.set(reactive_button_aria_context, reactive_button_label, None)
    })
  assert dom.outer_html(reactive_button_node)
    == "<button aria-atomic=\"true\" aria-current=\"page\" type=\"button\"></button>"
  dom.remove(reactive_button_mount)

  let html_specific_context = dataflow.dataflow()
  let html_specific_dir = dataflow.state(Some(html.HtmlDirLtr))
  let html_specific_button =
    html.button(html.plain_button(
      html.PlainButtonAttrs(
        ..html.plain_button_attrs(),
        dir: dataflow.mutation(html_specific_dir),
      ),
    ))
  let html_specific_mount =
    dom.mount_reactive(
      html_specific_context,
      document,
      body,
      html_specific_button,
    )
  let html_specific_node = dom.first(html_specific_mount)
  assert dom.outer_html(html_specific_node)
    == "<button dir=\"ltr\" type=\"button\"></button>"
  let assert Ok(Nil) =
    dataflow.txn(html_specific_context, fn() {
      dataflow.set(
        html_specific_context,
        html_specific_dir,
        Some(html.HtmlDirRtl),
      )
    })
  assert dom.outer_html(html_specific_node)
    == "<button dir=\"rtl\" type=\"button\"></button>"
  let assert Ok(Nil) =
    dataflow.txn(html_specific_context, fn() {
      dataflow.set(html_specific_context, html_specific_dir, None)
    })
  assert dom.outer_html(html_specific_node)
    == "<button type=\"button\"></button>"
  dom.remove(html_specific_mount)

  let disabled_context = dataflow.dataflow()
  let disabled = dataflow.state(Some(True))
  let disabled_button =
    html.button(html.plain_button(
      html.PlainButtonAttrs(
        ..html.plain_button_attrs(),
        disabled: dataflow.mutation(disabled),
      ),
    ))
  let disabled_mount =
    dom.mount_reactive(disabled_context, document, body, disabled_button)
  let disabled_node = dom.first(disabled_mount)
  assert dom.boolean_property(disabled_node, "disabled") == True
  assert dom.outer_html(disabled_node)
    == "<button disabled=\"\" type=\"button\"></button>"
  let assert Ok(Nil) =
    dataflow.txn(disabled_context, fn() {
      dataflow.set(disabled_context, disabled, Some(False))
    })
  assert dom.boolean_property(disabled_node, "disabled") == False
  assert dom.outer_html(disabled_node) == "<button type=\"button\"></button>"
  dom.remove(disabled_mount)

  let form_method_context = dataflow.dataflow()
  let form_method = dataflow.state(Some(html.FormGet))
  let form_method_button =
    html.button(html.submit_button(
      html.SubmitButtonAttrs(
        ..html.submit_button_attrs(),
        form_method: dataflow.mutation(form_method),
      ),
    ))
  let form_method_mount =
    dom.mount_reactive(form_method_context, document, body, form_method_button)
  let form_method_node = dom.first(form_method_mount)
  assert dom.outer_html(form_method_node)
    == "<button formmethod=\"get\" type=\"submit\"></button>"
  let assert Ok(Nil) =
    dataflow.txn(form_method_context, fn() {
      dataflow.set(form_method_context, form_method, Some(html.FormPost))
    })
  assert dom.outer_html(form_method_node)
    == "<button formmethod=\"post\" type=\"submit\"></button>"
  dom.remove(form_method_mount)

  let submitted_button =
    html.button(html.submit_button(
      html.SubmitButtonAttrs(
        ..html.submit_button_attrs(),
        form: dataflow.literal(Some("form-id")),
        form_action: dataflow.literal(Some("/submit")),
        form_enctype: dataflow.literal(Some(html.FormTextPlain)),
        form_no_validate: dataflow.literal(Some(True)),
        form_target: dataflow.literal(Some("_blank")),
        name: dataflow.literal(Some("action")),
        value: dataflow.literal(Some("save")),
      ),
    ))
  let submitted_button_mount = dom.mount(document, body, submitted_button)
  assert dom.outer_html(dom.first(submitted_button_mount))
    == "<button form=\"form-id\" formaction=\"/submit\" formenctype=\"text/plain\" formnovalidate=\"\" formtarget=\"_blank\" name=\"action\" value=\"save\" type=\"submit\"></button>"
  dom.remove(submitted_button_mount)

  let reset_button =
    html.button(html.reset_button(
      html.ResetButtonAttrs(
        ..html.reset_button_attrs(),
        form: dataflow.literal(Some("form-id")),
      ),
    ))
  let reset_button_mount = dom.mount(document, body, reset_button)
  assert dom.outer_html(dom.first(reset_button_mount))
    == "<button form=\"form-id\" type=\"reset\"></button>"
  dom.remove(reset_button_mount)

  let button_clicks = dataflow.state(0)
  let button =
    html.button(html.plain_button(
      html.PlainButtonAttrs(
        ..html.plain_button_attrs(),
        events: markup_attributes.Events(
          ..markup_attributes.events(),
          on_click: Some(
            markup_attributes.sync_listener(fn(event) {
              assert fragment.mouse_event_button(event) == 0
              let assert Ok(Nil) =
                dataflow.txn(event_context, fn() {
                  dataflow.set(event_context, button_clicks, 1)
                })
              Nil
            }),
          ),
        ),
      ),
    ))
  let button_mount = dom.mount(document, body, button)
  let button_node = dom.first(button_mount)
  dom.click(button_node)
  dom.wait(button_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(button_clicks)),
      )
      == 1
    Nil
  })

  let key_downs = dataflow.state(0)
  let keyboard_button =
    html.button(html.plain_button(
      html.PlainButtonAttrs(
        ..html.plain_button_attrs(),
        events: markup_attributes.Events(
          ..markup_attributes.events(),
          on_key_down: Some(
            markup_attributes.sync_listener(fn(event) {
              assert fragment.keyboard_event_key(event) == "Enter"
              let assert Ok(Nil) =
                dataflow.txn(event_context, fn() {
                  dataflow.set(event_context, key_downs, 1)
                })
              Nil
            }),
          ),
        ),
      ),
    ))
  let keyboard_button_mount = dom.mount(document, body, keyboard_button)
  let keyboard_button_node = dom.first(keyboard_button_mount)
  dom.dispatch_keyboard_event(keyboard_button_node, "keydown", "Enter")
  dom.wait(keyboard_button_mount, fn() {
    assert dataflow.value(dataflow.to_computation(dataflow.mutation(key_downs)))
      == 1
    Nil
  })
  dom.remove(keyboard_button_mount)
  dom.dispatch_keyboard_event(keyboard_button_node, "keydown", "Enter")
  dom.wait(keyboard_button_mount, fn() {
    assert dataflow.value(dataflow.to_computation(dataflow.mutation(key_downs)))
      == 1
    Nil
  })

  dom.remove(button_mount)
  dom.click(button_node)
  dom.wait(button_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(button_clicks)),
      )
      == 1
    Nil
  })

  let checkbox_clicks = dataflow.state(0)
  let checkbox =
    html.input(html.checkbox(
      html.CheckboxAttrs(
        ..html.checkbox_attrs(),
        events: markup_attributes.Events(
          ..markup_attributes.events(),
          on_click: Some(
            markup_attributes.sync_listener(fn(_event) {
              let assert Ok(Nil) =
                dataflow.txn(event_context, fn() {
                  dataflow.set(event_context, checkbox_clicks, 1)
                })
              Nil
            }),
          ),
        ),
      ),
    ))
  let checkbox_mount = dom.mount(document, body, checkbox)
  let checkbox_node = dom.first(checkbox_mount)
  dom.click(checkbox_node)
  dom.wait(checkbox_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(checkbox_clicks)),
      )
      == 1
    Nil
  })
  dom.remove(checkbox_mount)
  dom.click(checkbox_node)
  dom.wait(checkbox_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(checkbox_clicks)),
      )
      == 1
    Nil
  })

  let required_checkbox =
    html.input(html.checkbox(
      html.CheckboxAttrs(
        ..html.checkbox_attrs(),
        aria_disabled: dataflow.literal(Some(True)),
        form: dataflow.literal(Some("form-id")),
        required: dataflow.literal(Some(True)),
        title: dataflow.literal(Some("checkbox")),
      ),
    ))
  let required_checkbox_mount = dom.mount(document, body, required_checkbox)
  assert dom.outer_html(dom.first(required_checkbox_mount))
    == "<input aria-disabled=\"true\" title=\"checkbox\" form=\"form-id\" required=\"\" type=\"checkbox\">"
  dom.remove(required_checkbox_mount)

  let checkbox_context = dataflow.dataflow()
  let checkbox_required = dataflow.state(Some(True))
  let checkbox_checked = dataflow.state(Some(True))
  let checkbox_disabled = dataflow.state(Some(True))
  let reactive_checkbox =
    html.input(html.checkbox(
      html.CheckboxAttrs(
        ..html.checkbox_attrs(),
        required: dataflow.mutation(checkbox_required),
        checked: dataflow.mutation(checkbox_checked),
        disabled: dataflow.mutation(checkbox_disabled),
      ),
    ))
  let reactive_checkbox_mount =
    dom.mount_reactive(checkbox_context, document, body, reactive_checkbox)
  let reactive_checkbox_node = dom.first(reactive_checkbox_mount)
  assert dom.boolean_property(reactive_checkbox_node, "required") == True
  assert dom.boolean_property(reactive_checkbox_node, "checked") == True
  assert dom.boolean_property(reactive_checkbox_node, "disabled") == True
  let assert Ok(Nil) =
    dataflow.txn(checkbox_context, fn() {
      let assert Ok(Nil) =
        dataflow.set(checkbox_context, checkbox_required, Some(False))
      let assert Ok(Nil) =
        dataflow.set(checkbox_context, checkbox_checked, Some(False))
      dataflow.set(checkbox_context, checkbox_disabled, Some(False))
    })
  assert dom.boolean_property(reactive_checkbox_node, "required") == False
  assert dom.boolean_property(reactive_checkbox_node, "checked") == False
  assert dom.boolean_property(reactive_checkbox_node, "disabled") == False
  assert dom.outer_html(reactive_checkbox_node) == "<input type=\"checkbox\">"
  dom.remove(reactive_checkbox_mount)

  let text_input_clicks = dataflow.state(0)
  let text_input =
    html.input(html.text_input(
      html.TextInputAttrs(
        ..html.text_input_attrs(),
        events: markup_attributes.Events(
          ..markup_attributes.events(),
          on_click: Some(
            markup_attributes.sync_listener(fn(_event) {
              let assert Ok(Nil) =
                dataflow.txn(event_context, fn() {
                  dataflow.set(event_context, text_input_clicks, 1)
                })
              Nil
            }),
          ),
        ),
      ),
    ))
  let text_input_mount = dom.mount(document, body, text_input)
  let text_input_node = dom.first(text_input_mount)
  dom.click(text_input_node)
  dom.wait(text_input_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(text_input_clicks)),
      )
      == 1
    Nil
  })
  dom.remove(text_input_mount)
  dom.click(text_input_node)
  dom.wait(text_input_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(text_input_clicks)),
      )
      == 1
    Nil
  })

  let input =
    html.input(html.text_input(
      html.TextInputAttrs(
        ..html.text_input_attrs(),
        autocomplete: dataflow.literal(
          Some(
            html.AutocompleteTokens([
              html.AutocompleteSection("x"),
              html.AutocompleteEmail,
            ]),
          ),
        ),
        value: dataflow.literal(Some("x")),
      ),
    ))
  let input_mount = dom.mount(document, body, input)
  assert dom.outer_html(dom.first(input_mount))
    == "<input autocomplete=\"section-x email\" value=\"x\" type=\"text\">"
  dom.remove(input_mount)

  let input_content =
    html.input(html.text_input(
      html.TextInputAttrs(
        ..html.text_input_attrs(),
        content: fragment.TextInput("ignored by the HTML input element"),
      ),
    ))
  let input_content_mount = dom.mount(document, body, input_content)
  assert dom.text_content(dom.first(input_content_mount))
    == "ignored by the HTML input element"
  dom.remove(input_content_mount)

  let autocomplete_input =
    html.input(html.text_input(
      html.TextInputAttrs(
        ..html.text_input_attrs(),
        autocomplete: dataflow.literal(Some(html.AutocompleteOff)),
      ),
    ))
  let autocomplete_mount = dom.mount(document, body, autocomplete_input)
  assert dom.outer_html(dom.first(autocomplete_mount))
    == "<input autocomplete=\"off\" type=\"text\">"
  dom.remove(autocomplete_mount)

  let constrained_input =
    html.input(html.text_input(
      html.TextInputAttrs(
        ..html.text_input_attrs(),
        form: dataflow.literal(Some("form-id")),
        list: dataflow.literal(Some("suggestions")),
        min_length: dataflow.literal(Some(2)),
        pattern: dataflow.literal(Some("[a-z]+")),
      ),
    ))
  let constrained_input_mount = dom.mount(document, body, constrained_input)
  assert dom.outer_html(dom.first(constrained_input_mount))
    == "<input form=\"form-id\" list=\"suggestions\" minlength=\"2\" pattern=\"[a-z]+\" type=\"text\">"
  dom.remove(constrained_input_mount)

  let text_attributes_context = dataflow.dataflow()
  let text_id = dataflow.state(Some("text-a"))
  let text_value = dataflow.state(Some("a"))
  let text_required = dataflow.state(Some(True))
  let text_disabled = dataflow.state(Some(True))
  let reactive_text_input =
    html.input(html.text_input(
      html.TextInputAttrs(
        ..html.text_input_attrs(),
        id: dataflow.mutation(text_id),
        value: dataflow.mutation(text_value),
        required: dataflow.mutation(text_required),
        disabled: dataflow.mutation(text_disabled),
      ),
    ))
  let reactive_text_mount =
    dom.mount_reactive(
      text_attributes_context,
      document,
      body,
      reactive_text_input,
    )
  let reactive_text_node = dom.first(reactive_text_mount)
  assert dom.boolean_property(reactive_text_node, "required") == True
  assert dom.boolean_property(reactive_text_node, "disabled") == True
  assert dom.outer_html(reactive_text_node)
    == "<input id=\"text-a\" required=\"\" value=\"a\" disabled=\"\" type=\"text\">"
  let assert Ok(Nil) =
    dataflow.txn(text_attributes_context, fn() {
      let assert Ok(Nil) =
        dataflow.set(text_attributes_context, text_id, Some("text-b"))
      let assert Ok(Nil) =
        dataflow.set(text_attributes_context, text_value, Some("b"))
      let assert Ok(Nil) =
        dataflow.set(text_attributes_context, text_required, Some(False))
      dataflow.set(text_attributes_context, text_disabled, Some(False))
    })
  assert dom.boolean_property(reactive_text_node, "required") == False
  assert dom.boolean_property(reactive_text_node, "disabled") == False
  assert dom.outer_html(reactive_text_node)
    == "<input id=\"text-b\" value=\"b\" type=\"text\">"
  dom.remove(reactive_text_mount)

  let svg_event_count = dataflow.state(0)
  let svg_fragment =
    svg.svg(
      svg.SvgAttrs(
        ..svg.svg_attrs(),
        aria_disabled: dataflow.literal(Some(True)),
        nonce: dataflow.literal(Some("svg-nonce")),
        role: dataflow.literal(Some("img")),
        tab_index: dataflow.literal(Some(2)),
        autofocus: dataflow.literal(Some(True)),
        window_events: svg.WindowEvents(
          ..svg.window_events(),
          on_after_print: Some(
            markup_attributes.sync_listener(fn(_event) {
              let assert Ok(Nil) =
                dataflow.txn(event_context, fn() {
                  dataflow.set(event_context, svg_event_count, 1)
                })
              Nil
            }),
          ),
        ),
        preserve_aspect_ratio: dataflow.literal(
          Some(
            markup_data.svg_preserve_aspect_ratio(
              markup_data.SvgPreserveAspectRatioAttrs(
                align: markup_data.XMaxYMax,
                mode: Some(markup_data.Slice),
              ),
            ),
          ),
        ),
        view_box: dataflow.literal(
          Some(
            markup_data.rect(markup_data.RectAttrs(
              left: 1.0,
              top: 2.0,
              width: 3.0,
              height: 4.0,
            )),
          ),
        ),
        required_extensions: dataflow.literal(Some(["svg-root"])),
        system_language: dataflow.literal(Some(["en"])),
        height: dataflow.literal(Some(markup_data.SvgNumber(6.0))),
        width: dataflow.literal(Some(markup_data.SvgNumber(7.0))),
        x: dataflow.literal(Some(markup_data.SvgNumber(8.0))),
        y: dataflow.literal(Some(markup_data.SvgNumber(9.0))),
        content: fragment.FragmentInput(svg.rect(
          svg.RectAttrs(
            ..svg.rect_attrs(),
            height: dataflow.literal(Some(markup_data.SvgNumber(3.0))),
            width: dataflow.literal(Some(markup_data.SvgNumber(2.0))),
            x: dataflow.literal(Some(markup_data.SvgNumber(0.0))),
            y: dataflow.literal(Some(markup_data.SvgNumber(1.0))),
          ),
        )),
      ),
    )
  let svg_mount = dom.mount(document, body, svg_fragment)
  assert dom.outer_html(dom.first(svg_mount))
    == "<svg aria-disabled=\"true\" nonce=\"svg-nonce\" role=\"img\" tabindex=\"2\" autofocus=\"\" requiredExtensions=\"svg-root\" systemLanguage=\"en\" height=\"6\" preserveAspectRatio=\"xMaxYMax slice\" viewBox=\"1 2 3 4\" width=\"7\" x=\"8\" y=\"9\"><rect height=\"3\" width=\"2\" x=\"0\" y=\"1\"></rect></svg>"
  dom.dispatch_event(dom.first(svg_mount), "afterprint")
  dom.wait(svg_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(svg_event_count)),
      )
      == 1
    Nil
  })
  dom.remove(svg_mount)

  let reactive_root_context = dataflow.dataflow()
  let reactive_root_width = dataflow.state(Some(markup_data.SvgNumber(10.0)))
  let reactive_root_view_box =
    dataflow.state(
      Some(
        markup_data.rect(markup_data.RectAttrs(
          left: 0.0,
          top: 0.0,
          width: 1.0,
          height: 1.0,
        )),
      ),
    )
  let reactive_svg =
    svg.svg(
      svg.SvgAttrs(
        ..svg.svg_attrs(),
        width: dataflow.mutation(reactive_root_width),
        view_box: dataflow.mutation(reactive_root_view_box),
      ),
    )
  let reactive_svg_mount =
    dom.mount_reactive(reactive_root_context, document, body, reactive_svg)
  let reactive_svg_node = dom.first(reactive_svg_mount)
  assert dom.outer_html(reactive_svg_node)
    == "<svg viewBox=\"0 0 1 1\" width=\"10\"></svg>"
  let assert Ok(Nil) =
    dataflow.txn(reactive_root_context, fn() {
      let assert Ok(Nil) =
        dataflow.set(
          reactive_root_context,
          reactive_root_width,
          Some(markup_data.SvgNumber(11.0)),
        )
      dataflow.set(reactive_root_context, reactive_root_view_box, None)
    })
  assert dom.outer_html(reactive_svg_node) == "<svg width=\"11\"></svg>"
  dom.remove(reactive_svg_mount)

  let reactive_group_context = dataflow.dataflow()
  let reactive_group_extensions = dataflow.state(Some(["group-a", "group-b"]))
  let reactive_group =
    svg.g(
      svg.GAttrs(
        ..svg.g_attrs(),
        required_extensions: dataflow.mutation(reactive_group_extensions),
      ),
    )
  let reactive_group_mount =
    dom.mount_reactive(reactive_group_context, document, body, reactive_group)
  let reactive_group_node = dom.first(reactive_group_mount)
  assert dom.outer_html(reactive_group_node)
    == "<g requiredExtensions=\"group-a group-b\"></g>"
  let assert Ok(Nil) =
    dataflow.txn(reactive_group_context, fn() {
      dataflow.set(reactive_group_context, reactive_group_extensions, None)
    })
  assert dom.outer_html(reactive_group_node) == "<g></g>"
  dom.remove(reactive_group_mount)

  let reactive_group_aria_context = dataflow.dataflow()
  let reactive_group_label = dataflow.state(Some("group"))
  let reactive_group_aria =
    svg.g(
      svg.GAttrs(
        ..svg.g_attrs(),
        aria_atomic: dataflow.literal(Some(True)),
        aria_current: dataflow.literal(Some(html.AriaCurrentPage)),
        aria_label: dataflow.mutation(reactive_group_label),
      ),
    )
  let reactive_group_aria_mount =
    dom.mount_reactive(
      reactive_group_aria_context,
      document,
      body,
      reactive_group_aria,
    )
  let reactive_group_aria_node = dom.first(reactive_group_aria_mount)
  assert dom.outer_html(reactive_group_aria_node)
    == "<g aria-atomic=\"true\" aria-current=\"page\" aria-label=\"group\"></g>"
  let assert Ok(Nil) =
    dataflow.txn(reactive_group_aria_context, fn() {
      dataflow.set(reactive_group_aria_context, reactive_group_label, None)
    })
  assert dom.outer_html(reactive_group_aria_node)
    == "<g aria-atomic=\"true\" aria-current=\"page\"></g>"
  dom.remove(reactive_group_aria_mount)

  let reactive_svg_context = dataflow.dataflow()
  let reactive_width = dataflow.state(Some(markup_data.SvgNumber(2.0)))
  let reactive_extensions = dataflow.state(Some(["svg-a", "svg-b"]))
  let reactive_rect =
    svg.rect(
      svg.RectAttrs(
        ..svg.rect_attrs(),
        width: dataflow.mutation(reactive_width),
        required_extensions: dataflow.mutation(reactive_extensions),
      ),
    )
  let reactive_rect_mount =
    dom.mount_reactive(reactive_svg_context, document, body, reactive_rect)
  let reactive_rect_node = dom.first(reactive_rect_mount)
  assert dom.outer_html(reactive_rect_node)
    == "<rect requiredExtensions=\"svg-a svg-b\" width=\"2\"></rect>"
  let assert Ok(Nil) =
    dataflow.txn(reactive_svg_context, fn() {
      let assert Ok(Nil) =
        dataflow.set(
          reactive_svg_context,
          reactive_width,
          Some(markup_data.SvgNumber(4.0)),
        )
      dataflow.set(reactive_svg_context, reactive_extensions, None)
    })
  assert dom.outer_html(reactive_rect_node) == "<rect width=\"4\"></rect>"
  dom.remove(reactive_rect_mount)

  let math_fragment =
    math.math(
      math.MathAttrs(
        ..math.math_attrs(),
        aria_disabled: dataflow.literal(Some(True)),
        display: dataflow.literal(Some(math.MathInline)),
        nonce: dataflow.literal(Some("math-nonce")),
        role: dataflow.literal(Some("math")),
        tab_index: dataflow.literal(Some(3)),
        autofocus: dataflow.literal(Some(True)),
        content: fragment.FragmentInput(math.mi(
          math.MiAttrs(
            ..math.mi_attrs(),
            math_variant: dataflow.literal(Some(math.MathNormal)),
          ),
        )),
      ),
    )
  let math_mount = dom.mount(document, body, math_fragment)
  assert dom.outer_html(dom.first(math_mount))
    == "<math aria-disabled=\"true\" nonce=\"math-nonce\" role=\"math\" tabindex=\"3\" autofocus=\"\" display=\"inline\"><mi mathvariant=\"normal\"></mi></math>"
  dom.remove(math_mount)

  let operator =
    math.mo(
      math.MoAttrs(
        ..math.mo_attrs(),
        max_size: dataflow.literal(Some(math.MathLength(styling_data.px(11.0)))),
        operator_form: dataflow.literal(Some(math.FormPrefix)),
        stretchy: dataflow.literal(Some(True)),
      ),
    )
  let operator_mount = dom.mount(document, body, operator)
  assert dom.outer_html(dom.first(operator_mount))
    == "<mo maxsize=\"11px\" form=\"prefix\" stretchy=\"true\"></mo>"
  dom.remove(operator_mount)

  let reactive_math_context = dataflow.dataflow()
  let reactive_math_display = dataflow.state(Some(math.MathInline))
  let reactive_math_script_level = dataflow.state(Some(1))
  let reactive_math =
    math.math(
      math.MathAttrs(
        ..math.math_attrs(),
        display: dataflow.mutation(reactive_math_display),
        script_level: dataflow.mutation(reactive_math_script_level),
      ),
    )
  let reactive_math_mount =
    dom.mount_reactive(reactive_math_context, document, body, reactive_math)
  let reactive_math_node = dom.first(reactive_math_mount)
  assert dom.outer_html(reactive_math_node)
    == "<math display=\"inline\" scriptlevel=\"1\"></math>"
  let assert Ok(Nil) =
    dataflow.txn(reactive_math_context, fn() {
      let assert Ok(Nil) =
        dataflow.set(reactive_math_context, reactive_math_display, None)
      dataflow.set(reactive_math_context, reactive_math_script_level, Some(2))
    })
  assert dom.outer_html(reactive_math_node) == "<math scriptlevel=\"2\"></math>"
  dom.remove(reactive_math_mount)

  let reactive_math_aria_context = dataflow.dataflow()
  let reactive_math_label = dataflow.state(Some("formula"))
  let reactive_math_aria =
    math.math(
      math.MathAttrs(
        ..math.math_attrs(),
        aria_atomic: dataflow.literal(Some(True)),
        aria_current: dataflow.literal(Some(html.AriaCurrentPage)),
        aria_label: dataflow.mutation(reactive_math_label),
      ),
    )
  let reactive_math_aria_mount =
    dom.mount_reactive(
      reactive_math_aria_context,
      document,
      body,
      reactive_math_aria,
    )
  let reactive_math_aria_node = dom.first(reactive_math_aria_mount)
  assert dom.outer_html(reactive_math_aria_node)
    == "<math aria-atomic=\"true\" aria-current=\"page\" aria-label=\"formula\"></math>"
  let assert Ok(Nil) =
    dataflow.txn(reactive_math_aria_context, fn() {
      dataflow.set(reactive_math_aria_context, reactive_math_label, None)
    })
  assert dom.outer_html(reactive_math_aria_node)
    == "<math aria-atomic=\"true\" aria-current=\"page\"></math>"
  dom.remove(reactive_math_aria_mount)

  let style_host = markup_style.new(document)
  let style = background.size(background.Single(background.Contain), [])
  let style_fragment =
    html.div(html.DivAttrs(..html.div_attrs(), style: dataflow.literal(style)))
  let #(style_host, style_mount) =
    markup_style.mount(style_host, body, style_fragment)
  let style_node = dom.first(style_mount)
  assert dom.class_name(style_node) == "a0"
  assert dom.computed_style(style_node, "background-size") == "contain"

  let second_style_fragment =
    html.div(html.DivAttrs(..html.div_attrs(), style: dataflow.literal(style)))
  let #(_style_host, second_style_mount) =
    markup_style.mount(style_host, body, second_style_fragment)
  let second_style_node = dom.first(second_style_mount)
  assert dom.class_name(second_style_node) == "a0"
  assert style_layer_rule_count(document, 0) == 1

  let reactive_style_context = dataflow.dataflow()
  let reactive_style = dataflow.state(style)
  let reactive_style_fragment =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        style: dataflow.mutation(reactive_style),
      ),
    )
  let #(_style_host, reactive_style_mount) =
    markup_style.mount_reactive(
      reactive_style_context,
      style_host,
      body,
      reactive_style_fragment,
    )
  let reactive_style_node = dom.first(reactive_style_mount)
  assert dom.class_name(reactive_style_node) == "a0"
  let replacement_style =
    background.size(background.Single(background.Cover), [])
  let assert Ok(Nil) =
    dataflow.txn(reactive_style_context, fn() {
      dataflow.set(reactive_style_context, reactive_style, replacement_style)
    })
  assert dom.class_name(reactive_style_node) == "a1"
  assert dom.computed_style(reactive_style_node, "background-size") == "cover"
  assert style_layer_rule_count(document, 0) == 2
  let assert Ok(Nil) =
    dataflow.txn(reactive_style_context, fn() {
      dataflow.set(reactive_style_context, reactive_style, styling.empty())
    })
  assert dom.class_name(reactive_style_node) == ""
  dom.remove(reactive_style_mount)
  let assert Ok(Nil) =
    dataflow.txn(reactive_style_context, fn() {
      dataflow.set(reactive_style_context, reactive_style, replacement_style)
    })
  assert dom.class_name(reactive_style_node) == ""

  let context_custom =
    custom.define(fn(scoped, _attributes) {
      let scoped_document = markup_context.scoped_document(scoped)
      let context_body = dom.body(scoped_document)
      let target = dom.create_html_node(scoped_document, "section")
      dom.append_node(context_body, target)
      fragment.scoped_defer(scoped, fn() {
        dom.remove_node(context_body, target)
        Nil
      })
      let markup = markup_context.scoped_markup(scoped)
      let assert Some(layer) = markup_context.style_layer(markup)
      dom.insert_style_rule(layer, ".context-probe { color: red; }")
      fragment.FragmentInput(html.portal(
        target,
        html.DivAttrs(
          ..html.div_attrs(),
          class_names: dataflow.literal(Some(["context-probe"])),
          content: fragment.TextInput("context"),
        ),
      ))
    })
  let context_rule_count = style_layer_rule_count(document, 0)
  let #(_style_host, context_mount) =
    markup_style.mount(style_host, body, context_custom(#()))
  assert dom.text_content(body) == "context"
  assert style_layer_rule_count(document, 0) == context_rule_count + 1
  dom.remove(context_mount)
  assert dom.text_content(body) == ""

  let styled_condition_context = dataflow.dataflow()
  let styled_condition = dataflow.state(True)
  let styled_condition_mount =
    conditional.mount_styled(
      styled_condition_context,
      document,
      body,
      dataflow.mutation(styled_condition),
      html.div(
        html.DivAttrs(
          ..html.div_attrs(),
          style: dataflow.literal(replacement_style),
          content: fragment.TextInput("styled"),
        ),
      ),
      fragment.empty(),
      style_host,
    )
  assert dom.text_content(body) == "styled"
  let assert Ok(Nil) =
    dataflow.txn(styled_condition_context, fn() {
      dataflow.set(styled_condition_context, styled_condition, False)
    })
  assert dom.text_content(body) == ""
  conditional.remove(styled_condition_mount)

  let styled_each_context = dataflow.dataflow()
  let styled_items = dataflow.state(["a", "b"])
  let styled_each_mount =
    each.mount_styled(
      styled_each_context,
      document,
      body,
      dataflow.mutation(styled_items),
      fn(item, _index) { item },
      styled_each_item,
      style_host,
    )
  assert dom.text_content(body) == "ab"
  let assert Ok(Nil) =
    dataflow.txn(styled_each_context, fn() {
      dataflow.set(styled_each_context, styled_items, [])
    })
  assert dom.text_content(body) == ""
  each.remove(styled_each_mount)
  dom.remove(style_mount)
  dom.remove(second_style_mount)

  let portal_node = dom.create_html_node(document, "div")
  dom.set_text_content(portal_node, "q")
  dom.set_attribute_value(portal_node, "id", "x")
  dom.add_class(portal_node, "s")
  let portal_clicks = dataflow.state(0)
  let portal_context = dataflow.dataflow()
  let portal_show = dataflow.state(True)
  let portal_items = dataflow.state(["a", "b"])
  let portal_style =
    dataflow.state(
      background.color_typed(styling_typed_expr.rgb(
        styling_typed_expr.pct(styling_data.pct(100.0)),
        styling_typed_expr.pct(styling_data.pct(0.0)),
        styling_typed_expr.pct(styling_data.pct(0.0)),
      )),
    )
  let portal_tabindex = dataflow.state(Some(1))
  let portal_title = dataflow.state(None)
  let portal_dynamic_content =
    conditional.if_(
      dataflow.mutation(portal_show),
      fn(value) { value },
      fn(_condition) {
        fragment.FragmentInput(each.each(
          fn(state) {
            fragment.FragmentInput(html.span(
              html.DivAttrs(
                ..html.div_attrs(),
                content: fragment.ReactiveTextInput(dataflow.mutation(state)),
              ),
            ))
          },
          fn(item, _index) { item },
          dataflow.mutation(portal_items),
        ))
      },
      fn() { fragment.EmptyInput },
    )
  let portal_style_host = markup_style.new(document)
  let portal_fragment =
    html.portal(
      portal_node,
      html.DivAttrs(
        ..html.div_attrs(),
        id: dataflow.literal(Some("y")),
        style: dataflow.mutation(portal_style),
        events: markup_attributes.Events(
          ..markup_attributes.events(),
          on_click: Some(
            markup_attributes.sync_listener(fn(_event) {
              let assert Ok(Nil) =
                dataflow.set(portal_context, portal_clicks, 1)
              Nil
            }),
          ),
        ),
        tab_index: dataflow.mutation(portal_tabindex),
        title: dataflow.mutation(portal_title),
        content: fragment.ListInput([
          fragment.TextInput("_"),
          fragment.FragmentInput(portal_dynamic_content),
        ]),
      ),
    )
  let #(_portal_style_host, portal_mount) =
    markup_style.mount_reactive(
      portal_context,
      portal_style_host,
      body,
      portal_fragment,
    )
  assert dom.text_content(body) == ""
  assert dom.text_content(portal_node) == "q_ab"
  assert dom.class_name(portal_node) == "s a0"
  assert dom.has_attribute_value(portal_node, "tabindex")
  assert !dom.has_attribute_value(portal_node, "title")
  let assert Ok(Nil) =
    dataflow.txn(portal_context, fn() {
      dataflow.set(portal_context, portal_title, Some("portal"))
    })
  assert dom.has_attribute_value(portal_node, "title")
  let assert Ok(Nil) =
    dataflow.txn(portal_context, fn() {
      dataflow.set(portal_context, portal_title, None)
    })
  assert !dom.has_attribute_value(portal_node, "title")
  let assert Ok(Nil) =
    dataflow.txn(portal_context, fn() {
      dataflow.set(portal_context, portal_tabindex, None)
    })
  assert !dom.has_attribute_value(portal_node, "tabindex")
  let assert Ok(Nil) =
    dataflow.txn(portal_context, fn() {
      dataflow.set(portal_context, portal_tabindex, Some(2))
    })
  assert dom.has_attribute_value(portal_node, "tabindex")
  let assert Ok(Nil) =
    dataflow.txn(portal_context, fn() {
      dataflow.set(portal_context, portal_style, styling.empty())
    })
  assert dom.class_name(portal_node) == "s"
  let assert Ok(Nil) =
    dataflow.txn(portal_context, fn() {
      dataflow.set(portal_context, portal_style, replacement_style)
    })
  assert dom.class_name(portal_node) == "s a1"
  let assert Ok(Nil) =
    dataflow.txn(portal_context, fn() {
      dataflow.set(portal_context, portal_show, False)
    })
  assert dom.text_content(portal_node) == "q_"
  let assert Ok(Nil) =
    dataflow.txn(portal_context, fn() {
      let assert Ok(Nil) = dataflow.set(portal_context, portal_show, True)
      dataflow.set(portal_context, portal_items, ["b", "a", "c"])
    })
  assert dom.text_content(portal_node) == "q_bac"
  dom.click(portal_node)
  dom.wait(portal_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(portal_clicks)),
      )
      == 1
    Nil
  })
  dom.remove(portal_mount)
  assert dom.outer_html(portal_node) == "<div id=\"x\" class=\"s\">q</div>"
  let assert Ok(Nil) =
    dataflow.txn(portal_context, fn() {
      let assert Ok(Nil) = dataflow.set(portal_context, portal_show, False)
      dataflow.set(portal_context, portal_items, ["ignored"])
    })
  assert dom.outer_html(portal_node) == "<div id=\"x\" class=\"s\">q</div>"
  dom.click(portal_node)
  dom.wait(portal_mount, fn() {
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(portal_clicks)),
      )
      == 1
    Nil
  })

  let lazy_false_mount =
    dom.mount(
      document,
      body,
      conditional.if_(
        dataflow.literal(False),
        fn(value) { value },
        fn(_condition) { panic as "true branch was evaluated eagerly" },
        fn() { fragment.TextInput("lazy false") },
      ),
    )
  assert dom.text_content(body) == "lazy false"
  dom.remove(lazy_false_mount)

  let lazy_true_mount =
    dom.mount(
      document,
      body,
      conditional.if_(
        dataflow.literal(True),
        fn(value) { value },
        fn(_condition) { fragment.TextInput("lazy true") },
        fn() { panic as "false branch was evaluated eagerly" },
      ),
    )
  assert dom.text_content(body) == "lazy true"
  dom.remove(lazy_true_mount)

  let range_mount =
    dom.mount(
      document,
      body,
      markup_range.range(
        markup_range.RangeAttrs(content: [
          fragment.TextInput("a"),
          fragment.TextInput("b"),
        ]),
      ),
    )
  assert dom.text_content(body) == "ab"
  let assert [_, _] = dom.nodes(range_mount)
  dom.remove(range_mount)

  let static_text = reactive.text(document, body, dataflow.literal("static"))
  assert dom.outer_html(reactive.node(static_text)) == "static"
  reactive.remove(static_text)

  let context = dataflow.dataflow()
  let state = dataflow.state("a")
  let mounted_text = reactive.text(document, body, dataflow.mutation(state))
  assert dom.outer_html(reactive.node(mounted_text)) == "a"
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, "b") })
  assert dom.outer_html(reactive.node(mounted_text)) == "b"
  reactive.remove(mounted_text)
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, "c") })
  assert dom.outer_html(reactive.node(mounted_text)) == "b"

  let attribute_state = dataflow.state("a")
  let attribute_fragment = html.div(html.div_attrs())
  let attribute_mount = dom.mount(document, body, attribute_fragment)
  let attribute_node = dom.first(attribute_mount)
  let binding =
    reactive.attribute(attribute_node, "id", dataflow.mutation(attribute_state))
  assert dom.outer_html(attribute_node) == "<div id=\"a\"></div>"
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, attribute_state, "b") })
  assert dom.outer_html(attribute_node) == "<div id=\"b\"></div>"
  reactive.unbind_attribute(binding)
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, attribute_state, "c") })
  assert dom.outer_html(attribute_node) == "<div id=\"b\"></div>"

  let optional_state = dataflow.state(Some("a"))
  let optional_binding =
    reactive.optional_attribute(
      attribute_node,
      "title",
      dataflow.mutation(optional_state),
    )
  assert dom.outer_html(attribute_node) == "<div id=\"b\" title=\"a\"></div>"
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, optional_state, None) })
  assert dom.outer_html(attribute_node) == "<div id=\"b\"></div>"
  reactive.unbind_attribute(optional_binding)
  dom.remove(attribute_mount)

  let boolean_state = dataflow.state(True)
  let button_fragment =
    html.button(html.plain_button(html.plain_button_attrs()))
  let button_mount = dom.mount(document, body, button_fragment)
  let button_node = dom.first(button_mount)
  let boolean_binding =
    reactive.boolean_attribute(
      button_node,
      "disabled",
      dataflow.mutation(boolean_state),
    )
  let type_binding =
    reactive.attribute(button_node, "type", dataflow.literal("button"))
  assert dom.outer_html(button_node)
    == "<button type=\"button\" disabled=\"\"></button>"
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, boolean_state, False) })
  assert dom.outer_html(button_node) == "<button type=\"button\"></button>"
  reactive.unbind_attribute(boolean_binding)
  reactive.unbind_attribute(type_binding)
  let clicks = dataflow.state(0)
  let event_binding =
    reactive.on_value(
      button_node,
      "click",
      dataflow.mutation(clicks),
      fn(_event, count) {
        let assert Ok(Nil) =
          dataflow.txn(context, fn() {
            dataflow.set(context, clicks, count + 1)
          })
        Nil
      },
    )
  dom.click(button_node)
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(clicks))) == 1
  reactive.off(event_binding)
  dom.click(button_node)
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(clicks))) == 1
  dom.remove(button_mount)

  let reactive_context = dataflow.dataflow()
  let reactive_id = dataflow.state(Some("before"))
  let reactive_content = dataflow.state("one")
  let reactive_fragment =
    html.div(
      html.DivAttrs(
        ..html.div_attrs(),
        id: dataflow.mutation(reactive_id),
        content: fragment.ReactiveTextInput(dataflow.mutation(reactive_content)),
      ),
    )
  let reactive_mount =
    dom.mount_reactive(reactive_context, document, body, reactive_fragment)
  let reactive_node = dom.first(reactive_mount)
  assert dom.outer_html(reactive_node) == "<div id=\"before\">one</div>"
  let assert Ok(Nil) =
    dataflow.txn(reactive_context, fn() {
      let assert Ok(Nil) =
        dataflow.set(reactive_context, reactive_id, Some("after"))
      dataflow.set(reactive_context, reactive_content, "two")
    })
  assert dom.outer_html(reactive_node) == "<div id=\"after\">two</div>"
  dom.remove(reactive_mount)
  let assert Ok(Nil) =
    dataflow.txn(reactive_context, fn() {
      let assert Ok(Nil) =
        dataflow.set(reactive_context, reactive_id, Some("removed"))
      dataflow.set(reactive_context, reactive_content, "three")
    })
  assert dom.outer_html(reactive_node) == "<div id=\"after\">two</div>"

  let conditional_context = dataflow.dataflow()
  let condition = dataflow.state(True)
  let left = dom.create_text_node(document, "L")
  let right = dom.create_text_node(document, "R")
  dom.append_node(body, left)
  let mounted_conditional =
    conditional.mount(
      conditional_context,
      document,
      body,
      dataflow.mutation(condition),
      fragment.text("yes"),
      fragment.text("no"),
    )
  dom.append_node(body, right)
  assert dom.text_content(body) == "LyesR"
  let assert Ok(Nil) =
    dataflow.txn(conditional_context, fn() {
      dataflow.set(conditional_context, condition, False)
    })
  assert dom.text_content(body) == "LnoR"
  let assert Ok(Nil) =
    dataflow.txn(conditional_context, fn() {
      dataflow.set(conditional_context, condition, True)
    })
  assert dom.text_content(body) == "LyesR"
  conditional.remove(mounted_conditional)
  assert dom.text_content(body) == "LR"
  let assert Ok(Nil) =
    dataflow.txn(conditional_context, fn() {
      dataflow.set(conditional_context, condition, False)
    })
  assert dom.text_content(body) == "LR"
  dom.remove_node(body, left)
  dom.remove_node(body, right)
  assert dom.text_content(body) == ""

  let nested_context = dataflow.dataflow()
  let nested_condition = dataflow.state(True)
  let nested_text = dataflow.state("one")
  let nested =
    conditional.mount(
      nested_context,
      document,
      body,
      dataflow.mutation(nested_condition),
      fragment.reactive_text(dataflow.mutation(nested_text)),
      fragment.text("off"),
    )
  assert dom.text_content(body) == "one"
  let assert Ok(Nil) =
    dataflow.txn(nested_context, fn() {
      dataflow.set(nested_context, nested_text, "two")
    })
  assert dom.text_content(body) == "two"
  let assert Ok(Nil) =
    dataflow.txn(nested_context, fn() {
      dataflow.set(nested_context, nested_condition, False)
    })
  assert dom.text_content(body) == "off"
  let assert Ok(Nil) =
    dataflow.txn(nested_context, fn() {
      dataflow.set(nested_context, nested_text, "three")
    })
  assert dom.text_content(body) == "off"
  conditional.remove(nested)
  assert dom.text_content(body) == ""

  let option_context = dataflow.dataflow()
  let option_condition = dataflow.state(Some("first"))
  let option_branch_builds = dataflow.state(0)
  let option_fragment =
    conditional.if_some(
      dataflow.mutation(option_condition),
      fn(condition) {
        let build_count =
          dataflow.value(
            dataflow.to_computation(dataflow.mutation(option_branch_builds)),
          )
        let assert Ok(Nil) =
          dataflow.set(option_context, option_branch_builds, build_count + 1)
        fragment.ReactiveTextInput(dataflow.reactive(condition))
      },
      fn() { fragment.TextInput("off") },
    )
  let option_mount =
    dom.mount_reactive(option_context, document, body, option_fragment)
  assert dom.text_content(body) == "first"
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(option_branch_builds)),
    )
    == 1
  let assert Ok(Nil) =
    dataflow.txn(option_context, fn() {
      dataflow.set(option_context, option_condition, Some("second"))
    })
  assert dom.text_content(body) == "second"
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(option_branch_builds)),
    )
    == 1
  let assert Ok(Nil) =
    dataflow.txn(option_context, fn() {
      dataflow.set(option_context, option_condition, None)
    })
  assert dom.text_content(body) == "off"
  let assert Ok(Nil) =
    dataflow.txn(option_context, fn() {
      dataflow.set(option_context, option_condition, Some("third"))
    })
  assert dom.text_content(body) == "third"
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(option_branch_builds)),
    )
    == 2
  dom.remove(option_mount)

  let each_context = dataflow.dataflow()
  let items =
    dataflow.state([
      EachItem(0, "a"),
      EachItem(1, "b"),
      EachItem(2, "c"),
    ])
  let mounted_each =
    each.mount(
      each_context,
      document,
      body,
      dataflow.mutation(items),
      fn(item, _index) { item.id },
      each_item,
    )
  assert dom.text_content(body) == "abc"
  let assert Ok(Nil) =
    dataflow.txn(each_context, fn() {
      dataflow.set(each_context, items, [
        EachItem(0, "a"),
        EachItem(1, "b"),
        EachItem(2, "d"),
      ])
    })
  assert dom.text_content(body) == "abd"
  let assert Ok(Nil) =
    dataflow.txn(each_context, fn() {
      dataflow.set(each_context, items, [
        EachItem(2, "d"),
        EachItem(0, "a"),
        EachItem(1, "b"),
      ])
    })
  assert dom.text_content(body) == "dab"
  let assert Ok(Nil) =
    dataflow.txn(each_context, fn() {
      dataflow.set(each_context, items, [
        EachItem(2, "d"),
        EachItem(0, "a"),
      ])
    })
  assert dom.text_content(body) == "da"
  let assert Ok(Nil) =
    dataflow.txn(each_context, fn() {
      dataflow.set(each_context, items, [
        EachItem(2, "d"),
        EachItem(3, "e"),
        EachItem(0, "a"),
      ])
    })
  assert dom.text_content(body) == "dea"
  each.remove(mounted_each)
  assert dom.text_content(body) == ""
  let assert Ok(Nil) =
    dataflow.txn(each_context, fn() {
      dataflow.set(each_context, items, [EachItem(4, "ignored")])
    })
  assert dom.text_content(body) == ""

  let duplicate_host = dom.create_html_node(document, "div")
  dom.append_node(body, duplicate_host)
  let duplicate_error =
    call_and_catch(fn() {
      let _ =
        dom.mount(
          document,
          duplicate_host,
          each.each(
            fn(_state) { fragment.TextInput("x") },
            fn(_item, _index) { "duplicate" },
            dataflow.literal(["a", "b"]),
          ),
        )
      Nil
    })
  assert duplicate_error == "Duplicate key: \"duplicate\""
  dom.remove_node(body, duplicate_host)

  // Source `Map` keys use object identity. A fresh array for each item is
  // therefore not a duplicate, even though Gleam's structural equality
  // would consider the arrays equal.
  let identity_key_host = dom.create_html_node(document, "div")
  dom.append_node(body, identity_key_host)
  let identity_key_error =
    call_and_catch(fn() {
      let _ =
        dom.mount(
          document,
          identity_key_host,
          each.each(
            fn(_state) { fragment.TextInput("x") },
            fn(_item, _index) { [1] },
            dataflow.literal(["a", "b"]),
          ),
        )
      Nil
    })
  assert identity_key_error == ""
  assert dom.text_content(identity_key_host) == "xx"
  dom.remove_node(body, identity_key_host)

  // SameValueZero also makes NaN a stable key. Updating the list with a new
  // NaN must retain the mounted fragment rather than remove and recreate it.
  let nan_context = dataflow.dataflow()
  let nan_mount_count = dataflow.state(0)
  let nan_items = dataflow.state([nan()])
  let nan_host = dom.create_html_node(document, "div")
  dom.append_node(body, nan_host)
  let nan_mounted =
    each.mount(
      nan_context,
      document,
      nan_host,
      dataflow.mutation(nan_items),
      fn(_item, _index) { nan() },
      fn(_state) {
        html.div(
          html.DivAttrs(
            ..html.div_attrs(),
            on_add: Some(
              fragment.sync_on_add(fn(_event) {
                let current =
                  dataflow.value(
                    dataflow.to_computation(dataflow.mutation(nan_mount_count)),
                  )
                let assert Ok(Nil) =
                  dataflow.txn(nan_context, fn() {
                    dataflow.set(nan_context, nan_mount_count, current + 1)
                  })
                Nil
              }),
            ),
            content: fragment.TextInput("n"),
          ),
        )
      },
    )
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(nan_mount_count)),
    )
    == 1
  let assert Ok(Nil) =
    dataflow.txn(nan_context, fn() {
      dataflow.set(nan_context, nan_items, [nan()])
    })
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(nan_mount_count)),
    )
    == 1
  assert dom.text_content(nan_host) == "n"
  each.remove(nan_mounted)
  dom.remove_node(body, nan_host)

  let async_context = dataflow.dataflow()
  let async_host = dom.create_html_node(document, "div")
  dom.append_node(body, async_host)
  let async_custom =
    custom.define_async(fn(_scoped, _attributes: String) {
      fn(done) {
        set_timeout(fn() { done(Ok(fragment.TextInput("late"))) }, 0.0)
      }
    })
  let async_mount =
    dom.mount_reactive(async_context, document, async_host, async_custom("x"))
  assert dom.text_content(async_host) == ""

  let cancelled_removed = dataflow.state(False)
  let cancelled_custom =
    custom.define_async(fn(scoped, _attributes: String) {
      fragment.scoped_defer(scoped, fn() {
        let assert Ok(Nil) =
          dataflow.txn(async_context, fn() {
            dataflow.set(async_context, cancelled_removed, True)
          })
        Nil
      })
      fn(done) {
        set_timeout(
          fn() { done(Ok(fragment.TextInput("must not mount"))) },
          10.0,
        )
      }
    })
  let cancelled_mount =
    dom.mount_reactive(
      async_context,
      document,
      async_host,
      cancelled_custom("x"),
    )
  dom.remove(cancelled_mount)
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(cancelled_removed)),
    )
    == True

  let rejected_host = dom.create_html_node(document, "div")
  dom.append_node(body, rejected_host)
  let rejected_custom =
    custom.define_async(fn(_scoped, _attributes: String) {
      fn(done) { done(Error("source custom rejection")) }
    })
  let rejected_mount =
    dom.mount_reactive(
      dataflow.dataflow(),
      document,
      rejected_host,
      rejected_custom("x"),
    )

  let multi_two_context = dataflow.dataflow()
  let multi_two_first = dataflow.state("a")
  let multi_two_second = dataflow.state("b")
  let multi_two_observed = dataflow.state("")
  let multi_two_host = dom.create_html_node(document, "div")
  dom.append_node(body, multi_two_host)
  let multi_two_custom =
    custom.define_async(fn(scoped, _attributes: String) {
      fn(done) {
        fragment.scoped_effect_async2(
          scoped,
          fn(first, second) {
            fn(effect_done) {
              let assert Ok(Nil) =
                dataflow.txn(multi_two_context, fn() {
                  dataflow.set(
                    multi_two_context,
                    multi_two_observed,
                    first <> second,
                  )
                })
              effect_done(Ok(Nil))
            }
          },
          dataflow.to_computation(dataflow.mutation(multi_two_first)),
          dataflow.to_computation(dataflow.mutation(multi_two_second)),
        )(fn(result) {
          case result {
            Error(reason) -> done(Error(reason))
            Ok(_) -> done(Ok(fragment.TextInput("two")))
          }
        })
      }
    })
  let multi_two_mount =
    dom.mount_reactive(
      multi_two_context,
      document,
      multi_two_host,
      multi_two_custom("x"),
    )

  let multi_three_context = dataflow.dataflow()
  let multi_three_first = dataflow.state("a")
  let multi_three_second = dataflow.state("b")
  let multi_three_third = dataflow.state("c")
  let multi_three_observed = dataflow.state("")
  let multi_three_host = dom.create_html_node(document, "div")
  dom.append_node(body, multi_three_host)
  let multi_three_custom =
    custom.define_async(fn(scoped, _attributes: String) {
      fn(done) {
        fragment.scoped_effect_async3(
          scoped,
          fn(first, second, third) {
            fn(effect_done) {
              let assert Ok(Nil) =
                dataflow.txn(multi_three_context, fn() {
                  dataflow.set(
                    multi_three_context,
                    multi_three_observed,
                    first <> second <> third,
                  )
                })
              effect_done(Ok(Nil))
            }
          },
          dataflow.to_computation(dataflow.mutation(multi_three_first)),
          dataflow.to_computation(dataflow.mutation(multi_three_second)),
          dataflow.to_computation(dataflow.mutation(multi_three_third)),
        )(fn(result) {
          case result {
            Error(reason) -> done(Error(reason))
            Ok(_) -> done(Ok(fragment.TextInput("three")))
          }
        })
      }
    })
  let multi_three_mount =
    dom.mount_reactive(
      multi_three_context,
      document,
      multi_three_host,
      multi_three_custom("x"),
    )

  let scoped_effect_context = dataflow.dataflow()
  let scoped_effect_title = dataflow.state("a")
  let scoped_effect_observed = dataflow.state("")
  let scoped_effect_host = dom.create_html_node(document, "div")
  dom.append_node(body, scoped_effect_host)
  let scoped_effect_custom =
    custom.define_async(fn(scoped, content: dataflow.NodeOpt(String)) {
      fn(done) {
        fragment.scoped_effect_async(
          scoped,
          fn(value) {
            fn(effect_done) {
              let assert Ok(Nil) =
                dataflow.txn(scoped_effect_context, fn() {
                  dataflow.set(
                    scoped_effect_context,
                    scoped_effect_observed,
                    value,
                  )
                })
              effect_done(Ok(Nil))
            }
          },
          dataflow.to_computation(content),
        )(fn(result) {
          case result {
            Error(reason) -> done(Error(reason))
            Ok(_) -> done(Ok(fragment.ReactiveTextInput(content)))
          }
        })
      }
    })
  let scoped_effect_mount =
    dom.mount_reactive(
      scoped_effect_context,
      document,
      scoped_effect_host,
      scoped_effect_custom(dataflow.mutation(scoped_effect_title)),
    )
  assert dom.text_content(scoped_effect_host) == ""

  dom.wait(async_mount, fn() {
    assert dom.text_content(async_host) == "late"
    dom.remove(async_mount)
    Nil
  })
  dom.wait(cancelled_mount, fn() {
    assert dom.text_content(async_host) == ""
    dom.remove_node(body, async_host)
    Nil
  })
  dom.wait(scoped_effect_mount, fn() {
    assert dom.text_content(scoped_effect_host) == "a"
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(scoped_effect_observed)),
      )
      == "a"
    let assert Ok(Nil) =
      dataflow.txn(scoped_effect_context, fn() {
        dataflow.set(scoped_effect_context, scoped_effect_title, "b")
      })
    dom.wait(scoped_effect_mount, fn() {
      assert dom.text_content(scoped_effect_host) == "b"
      assert dataflow.value(
          dataflow.to_computation(dataflow.mutation(scoped_effect_observed)),
        )
        == "b"
      dom.remove(scoped_effect_mount)
      dom.remove_node(body, scoped_effect_host)
      Nil
    })
    Nil
  })
  dom.wait(multi_two_mount, fn() {
    assert dom.text_content(multi_two_host) == "two"
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(multi_two_observed)),
      )
      == "ab"
    assert dom.text_content(multi_three_host) == "three"
    assert dataflow.value(
        dataflow.to_computation(dataflow.mutation(multi_three_observed)),
      )
      == "abc"
    let assert Ok(Nil) =
      dataflow.txn(multi_two_context, fn() {
        let assert Ok(Nil) =
          dataflow.set(multi_two_context, multi_two_first, "x")
        dataflow.set(multi_two_context, multi_two_second, "y")
      })
    dom.wait(multi_two_mount, fn() {
      assert dataflow.value(
          dataflow.to_computation(dataflow.mutation(multi_two_observed)),
        )
        == "xy"
      dom.remove(multi_two_mount)
      dom.remove(multi_three_mount)
      dom.remove_node(body, multi_two_host)
      dom.remove_node(body, multi_three_host)
      Nil
    })
    Nil
  })
  dom.wait_result(rejected_mount, fn(result) {
    assert result == Error("source custom rejection")
    dom.remove(rejected_mount)
    dom.remove_node(body, rejected_host)
    Nil
  })

  io.println("lib-markup Chromium DOM FFI passed")
}

fn each_item(state: dataflow.Mutation(EachItem)) -> fragment.Fragment {
  let name =
    dataflow.map(fn(item: EachItem) { item.name }, dataflow.mutation(state))
  html.div(
    html.DivAttrs(
      ..html.div_attrs(),
      content: fragment.FragmentInput(
        fragment.reactive_text(dataflow.reactive(name)),
      ),
    ),
  )
}

fn styled_each_item(state: dataflow.Mutation(String)) -> fragment.Fragment {
  html.div(
    html.DivAttrs(
      ..html.div_attrs(),
      style: dataflow.literal(
        background.size(background.Single(background.Contain), []),
      ),
      content: fragment.ReactiveTextInput(dataflow.mutation(state)),
    ),
  )
}

import dev_browser_test as browser
import gleam/io
import gleam/string

const styles_module = "/symbolize_svc_auth_guest_view/svc_auth_guest_view_styles_browser_test_main.mjs"

const view_module = "/symbolize_svc_auth_guest_view/svc_auth_guest_view_browser_test_main.mjs"

const worker_module = "/symbolize_svc_auth_guest_view/svc_auth_guest_view_browser_test_worker_main.mjs"

type View {
  View(
    title: String,
    body: String,
    accent: String,
    position: String,
    min_height: String,
    overflow_x: String,
    counter: String,
    svg_namespace: String,
    rect_class: String,
    math_namespace: String,
    nonce: String,
    math_text: String,
    input: String,
    checked: Bool,
    counter_after: String,
    counter_class_after: String,
  )
}

pub fn main() {
  let root = browser.resolve_path("build/dev/javascript")
  browser.run(
    fn(path, _search, _request, response) {
      case path {
        "/" ->
          browser.reply_text(
            response,
            200,
            "text/html",
            "<!doctype html><title>Existing title</title>",
          )
        "/favicon.ico" -> browser.reply_text(response, 404, "text/plain", "")
        "/svc-auth-guest-view-worker.mjs" ->
          browser.reply_text(
            response,
            200,
            "text/javascript",
            browser.module_script(worker_module, "main"),
          )
        _ -> browser.serve_static(root, path, response)
      }
    },
    operation,
  )
}

fn operation(
  page: browser.Page,
  server: browser.Server,
  origin: String,
) -> browser.Async(Nil) {
  browser.bind(browser.call_module(page, styles_module, []), fn(_ignored) {
    browser.bind(
      browser.call_module(page, view_module, [
        origin <> "/svc-auth-guest-view-worker.mjs",
      ]),
      fn(_ignored) {
        browser.bind(browser.wait(5000), fn(_ignored) {
          browser.bind(read_initial(page), fn(initial) {
            browser.bind(
              browser.click(page, "body > :nth-child(3)"),
              fn(_ignored) {
                browser.bind(browser.wait(100), fn(_ignored) {
                  browser.bind(
                    browser.element_text(page, "body > :nth-child(3)"),
                    fn(counter_after) {
                      browser.bind(
                        browser.element_attribute(
                          page,
                          "body > :nth-child(3)",
                          "class",
                        ),
                        fn(counter_class_after) {
                          fn(done) {
                            done(
                              case
                                check_view(View(
                                  initial.title,
                                  initial.body,
                                  initial.accent,
                                  initial.position,
                                  initial.min_height,
                                  initial.overflow_x,
                                  initial.counter,
                                  initial.svg_namespace,
                                  initial.rect_class,
                                  initial.math_namespace,
                                  initial.nonce,
                                  initial.math_text,
                                  initial.input,
                                  initial.checked,
                                  counter_after,
                                  counter_class_after,
                                ))
                              {
                                Error(reason) -> Error(reason)
                                Ok(Nil) ->
                                  case browser.report(page, server, [
                                    "svc-auth-guest-view custom content mounted",
                                    "client data pong",
                                  ]) {
                                    Error(reason) -> Error(reason)
                                    Ok(Nil) -> {
                                      io.println(
                                        "svc-auth-guest-view Chromium Gleam driver passed",
                                      )
                                      Ok(Nil)
                                    }
                                  }
                              },
                            )
                          }
                        },
                      )
                    },
                  )
                })
              },
            )
          })
        })
      },
    )
  })
}

fn read_initial(page: browser.Page) -> browser.Async(View) {
  browser.bind(browser.document_title(page), fn(title) {
    browser.bind(browser.body_text(page), fn(body) {
      browser.bind(
        browser.computed_style(page, "html", "accent-color"),
        fn(accent) {
          browser.bind(
            browser.computed_style(page, "body", "position"),
            fn(position) {
              browser.bind(
                browser.computed_style(page, "body", "min-height"),
                fn(min_height) {
                  browser.bind(
                    browser.computed_style(page, "body", "overflow-x"),
                    fn(overflow_x) {
                      browser.bind(
                        browser.element_text(page, "body > :nth-child(3)"),
                        fn(counter) {
                          browser.bind(
                            browser.element_namespace(
                              page,
                              "body > :nth-child(4)",
                            ),
                            fn(svg_namespace) {
                              browser.bind(
                                browser.element_attribute(
                                  page,
                                  "body > :nth-child(4) rect",
                                  "class",
                                ),
                                fn(rect_class) {
                                  browser.bind(
                                    browser.element_namespace(
                                      page,
                                      "body > :nth-child(5)",
                                    ),
                                    fn(math_namespace) {
                                      browser.bind(
                                        browser.element_attribute(
                                          page,
                                          "body > :nth-child(5)",
                                          "nonce",
                                        ),
                                        fn(nonce) {
                                          browser.bind(
                                            browser.element_text(
                                              page,
                                              "body > :nth-child(5)",
                                            ),
                                            fn(math_text) {
                                              browser.bind(
                                                browser.element_value(
                                                  page,
                                                  "body > :nth-child(6) input[type=text]",
                                                ),
                                                fn(input) {
                                                  browser.bind(
                                                    browser.element_checked(
                                                      page,
                                                      "body > :nth-child(6) input[type=checkbox]",
                                                    ),
                                                    fn(checked) {
                                                      fn(done) {
                                                        done(
                                                          Ok(View(
                                                            title,
                                                            body,
                                                            accent,
                                                            position,
                                                            min_height,
                                                            overflow_x,
                                                            counter,
                                                            svg_namespace,
                                                            rect_class,
                                                            math_namespace,
                                                            nonce,
                                                            math_text,
                                                            input,
                                                            checked,
                                                            "",
                                                            "",
                                                          )),
                                                        )
                                                      }
                                                    },
                                                  )
                                                },
                                              )
                                            },
                                          )
                                        },
                                      )
                                    },
                                  )
                                },
                              )
                            },
                          )
                        },
                      )
                    },
                  )
                },
              )
            },
          )
        },
      )
    })
  })
}

fn check_view(view: View) -> Result(Nil, String) {
  case view {
    View(
      "Symbolize Custom",
      body,
      "rgb(255, 0, 255)",
      "relative",
      min_height,
      "hidden",
      "st ffb hello / 0",
      "http://www.w3.org/2000/svg",
      rect_class,
      "http://www.w3.org/1998/Math/MathML",
      "x",
      "x+y",
      "abc",
      True,
      "st ffb hello / 1",
      counter_class_after,
    ) ->
      case
        string.contains(body, "The Tale of Peter Rabbit")
        && min_height != "0px"
        && rect_class != ""
        && counter_class_after != ""
      {
        True -> Ok(Nil)
        False -> Error("unexpected auth guest view")
      }
    _ -> Error("unexpected auth guest view")
  }
}

import gleam/io
import lib_markup_dom as dom
import lib_markup_fragment as fragment
import lib_markup_style as markup_style
import svc_auth_guest_view_styles as styles

pub fn main() {
  let document = dom.document()
  let body = dom.body(document)
  let html = dom.document_element(document)
  let host = markup_style.new(document)
  let html_style = styles.html_style(document)
  let #(host, body_style) = styles.body_style(host, document)
  let value =
    fragment.range([
      fragment.FragmentInput(html_style),
      fragment.FragmentInput(body_style),
    ])
  let #(_host, mounted) = markup_style.mount(host, body, value)

  assert dom.class_name(html) != ""
  assert dom.class_name(body) != ""
  assert dom.computed_style(html, "accent-color") == "rgb(255, 0, 255)"
  assert dom.computed_style(body, "position") == "relative"
  assert dom.computed_style(body, "min-height") != ""
  dom.remove(mounted)
  assert dom.class_name(html) == ""
  assert dom.class_name(body) == ""
  io.println("svc-auth-guest-view CSS setup Chromium parity passed")
}

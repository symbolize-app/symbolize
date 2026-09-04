export function document() {
  return globalThis.document
}

export function body(document) {
  return document.body
}

export function document_element(document) {
  return document.documentElement
}

export function head(document) {
  return document.head
}

export function remove_first_title(node) {
  node.querySelector('title')?.remove()
}

export function create_element(document, tag) {
  return document.createElement(tag)
}

export function create_element_ns(document, namespaceUri, tag) {
  return document.createElementNS(namespaceUri, tag)
}

export function create_text(document, value) {
  return document.createTextNode(value)
}

export function create_comment(document, value) {
  return document.createComment(value)
}

export function set_attribute(node, name, value) {
  node.setAttribute(name, value)
}

export function remove_attribute(node, name) {
  node.removeAttribute(name)
}

export function has_attribute(node, name) {
  return node.hasAttribute(name)
}

export function boolean_property(node, name) {
  return Boolean(node[name])
}

export function get_attribute_value(node, name) {
  return node.getAttribute(name) ?? ''
}

export function append_child(parent, child) {
  parent.append(child)
}

export function remove_child(parent, child) {
  if (child.parentNode === parent) parent.removeChild(child)
}

export function set_text_content(node, value) {
  node.textContent = value
}

export function outer_html(node) {
  return node.outerHTML ?? node.textContent ?? ''
}

export function is_connected(node) {
  return node.isConnected
}

export function add_event_listener(node, name, callback) {
  node.addEventListener(name, callback)
  return { node, name, callback }
}

export function remove_event_listener(subscription) {
  const { node, name, callback } = subscription
  node.removeEventListener(name, callback)
}

export function click(node) {
  node.click()
}

export function dispatch_event(node, name) {
  node.dispatchEvent(new Event(name))
}

export function dispatch_keyboard_event(node, name, key) {
  node.dispatchEvent(new KeyboardEvent(name, { key, bubbles: true }))
}

export function call_and_catch(callback) {
  try {
    callback()
    return ''
  } catch (error) {
    return String(error?.message ?? error)
  }
}

export function cast_event(event) {
  return event
}

export function mouse_event_button(event) {
  return event.button
}

export function keyboard_event_key(event) {
  return event.key
}

export function new_style_layer(document) {
  const style = document.createElement('style')
  document.head.append(style)
  if (!style.sheet) throw new Error('No style sheet')
  const index = style.sheet.insertRule('@layer base {}')
  return style.sheet.cssRules.item(index)
}

export function insert_style_rule(layer, code) {
  layer.insertRule(code, layer.cssRules.length)
}

export function style_layer_rule_count(document, index) {
  const style = document.head.querySelectorAll('style').item(index)
  const layer = style?.sheet?.cssRules.item(0)
  return layer?.cssRules?.length ?? -1
}

export function add_class(node, name) {
  node.classList.add(name)
}

export function remove_class(node, name) {
  node.classList.remove(name)
}

export function class_name(node) {
  return node.className
}

export function computed_style(node, property) {
  return globalThis
    .getComputedStyle(node)
    .getPropertyValue(property)
    .trim()
}

export function insert_before(parent, child, before) {
  parent.insertBefore(child, before)
}

export function text_content(node) {
  return node.textContent ?? ''
}

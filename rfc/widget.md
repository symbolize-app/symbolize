# Widget

## Snippet

```ts
export const myButton = defineWidget({
  context: defineWidgetContext<TrackingContext>(),
  properties: defineWidgetProperties<{
    text: string,
    primary: boolean
  }>(),
  defaults: defineWidgetDefaults({
    primary: false
  }),
  events: defineWidgetEvents<{
    click: [modifiers: Modifier[]]
  }>(),
  init(ctx, widget) {
    const internalButton = button(ctx, {
      style: [
        myButtonStyle,
      ],
      on: {
        click: onClick,
      }
    })
    
    widget({
      content: [internalButton],
      watch: {
        text: watchText,
      }
    })
    
    function onClick() {
      widget.emit.click([Modifier.shift])
    }
    
    function watchText() {
      internalButton({
        content: [widget.text],
      })
    }
    
    function focus() {
      internalButton.focus()
    }
    
    return { 
      focus
    }
  }
})
```

## Typing

The `widget` passed to `init` won't have types for the methods (returned from `init`), but technically it will be the same object (methods are attached after.

Special function merges properties and defaults to build required init properties.

## Watchers

For required properties, still also call the watcher after init.

## Lifecycle

Make a path for subscribing and unsbubscribing to global events. This can be done with a generic custom element's connected/disconnected callback.

Then make helpers for common scenarios -- global `addEventListener`, `MutationObserver`, `IntersectionObserver`.

## SSR

Try out some alternatives to jsdom: 
- https://github.com/WebReflection/linkedom
- https://github.com/capricorn86/happy-dom

## Debugging

Make a SWC AST transformer plugin to add ownership info to the DOM (either all widget DOM elements or just the root ones). Could be comments or data attributes, including symbol name, file name, line number. May want to stack contexts for when a virtual widget only contains other virtual widgets.

## Context

Each context has its own symbol key, to avoid any chance of contexts conflicting.

### Headers

All header usage is encouraged to be relative. The absolute level will be controlled via context.

## Non-widgets

A bunch of the code (watchers, event emitters, contexts, etc.) should be defined outside of widgets and usable from non-widget services. For example, 

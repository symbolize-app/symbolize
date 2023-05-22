# Style

## Goals

- Completely independent from widget
  - Widget should be able to consume the basic API
- Compose classes and apply them all without duplicating rules
- Physics-based animations

## Objects

- Style
- Variable
- Keyframes
- Data attribute
- Animation curves

## CSS feature support

- Media queries
- Support queries
- States
- Global reset

## Layers

Not sure if they'll really be helpful for resolving specificity, but they'll be good for avoiding global state. Each layer needs a symbol, which will act as the class prefix.

Within a layer, make sure when each style gets compiled, it gets inserted into the stylesheet in the order it was found in code. This will preserve developer model of specificity (later defined styles overriding newer ones).

## SSR

Compile all of a microfrontend's referenced styles to CSS in SSR. For CSR, maybe lazily compile the styles if it's better for page performance.

## Debugging

Would be nice to have an SWC plugin that puts style symbol names into class names or variables or comments.

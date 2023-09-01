# Style

## Goals

- Completely independent from widget
  - Widget should be able to consume the basic API
- Typed properties and fluent values
- Use style declarations inline with widgets
- Physics-based animations

## Objects

- Declaration
- Variable
- Keyframes
- Data attribute
- Animation curves

## CSS feature support

- Media queries
- Support queries
- States
- Global reset

## Multi-delaration rules

Compared to utility class frameworks, here all declarations used together will get compiled to a single CSS class. Even if declarations are grouped together (for reuse), this will not trigger multiple classes. Changing the set or order of declarations after initial render will also cause compilation of a new CSS class.

Because declaration lists are essentially dynamic, there needs to be an efficient way to look up a matching (already compiled) CSS class. To do this, delcarations (and declaration groups) will be "interned", and the final result will be a lookup of the root declaration list as a group. A special map (that accepts tuples) will be needed for group interning.

## Layers

CSS layers can be really be helpful for resolving specificity, and they'll be good for avoiding global state. Each layer needs a symbol, which will also act as the class prefix.

Within a layer, make sure when each style gets compiled, it gets inserted into the stylesheet in the order it was found in code. This will preserve developer model of specificity (later defined styles overriding newer ones).

## Lazy rendering

For SSR and CSR, build a unique stylesheet that includes just the classes created by the active components.

## Debugging

Would be nice to have an SWC plugin that puts style symbol names into class names or variables or comments.

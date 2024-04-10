# Web view

Main interactive interface here built on HTML / web tech.

## Overall goals

- No external dependencies
- No custom syntax, just plain TypeScript
- Direct access to web platform features

## Reactivity

- Only run computations if inputs have changed
- Only run computations on demand, or if subscribed
- Use async nested transactions for state updates

## View building

- Custom fragments are synthetic, no DOM node involved
- Support custom attribute APIs that resemble DOM attribute APIs
- Ranges are also fragments
- Comments used to delimit dynamic/conditional fragments
- Syntax shortcuts for common operations where possible
- Stronger TypeScript types than builtin library
- Write using attributes, for easy simulated DOM compatibility
- Allow removing any attribute
- Events are serialized in async, reactive transactions

## Styling

- Composed via simple functions, atoms indistinguishable from helpers
- Each property-value combination is compiled to one rule
- Each rule can have multiple selectors (conditions & specificity)
- Atoms only cover truly atomic properties, not compound ones
- Overriding an atom adds a class with higher specificity (`:not(#\#)`)
- All rule composition operations are interned
- No nesting/grouping of styles within a condition
- Property value is where conditions get applied
- Strongly-typed IDs for CSS custom variables, CSS animations, DOM data attributes
- Styles written as usage (no external definition), for easy refactoring
- Physics-based animations
- Global style reset

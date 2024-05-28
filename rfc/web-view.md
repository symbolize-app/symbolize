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

### Future optimization ideas

- Queue DOM effects to apply in batches
- Preallocate and reuse DOM objects using pools

## Styling

### API

- As strongly typed as possible, at the expense of completeness
- Composed via simple functions, atoms indistinguishable from helpers
- Property value is where conditions get applied
- Styles written as usage (no external definition), for easy refactoring
- Auto-generated unique IDs for CSS animations, CSS custom properties, and DOM data attributes
- Custom properties when inline accept reactive data
- Pass normal styles, inline properties, and data attributes all together for easy overrides

### CSS generation

- Atoms only cover truly atomic properties, not shorthand properties
- Overriding an atom causes the overridden atom to be omitted
- Null value will also omit an atom, without replacement
- Each property-value combination is compiled to one rule with multiple conditions nested inside
- Conditions are all contained inside `:where()` pseudo-classes, so that code ordering of conditions directly translates to cascade

### Other

- Parent selectors to avoid repeating data attributes
- Physics-based animations
- Global style reset

### Future optimization ideas

- Allow targeting child elements that don't have their own classes
- Allow precompiling styles outside of fragments
- Pre-process all styles as macros

### Future changes

- Set up strict nulls for CSS expressions
  - Chained conditions can't resolve to empty
  - Multiple values can't be empty
  - Computations can't be null

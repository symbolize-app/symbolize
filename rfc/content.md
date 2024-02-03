# Content system

## Tasks

- [ ] Syntax for inlines with parameters
  - e.g. `` `spoiler`[text]``
  - e.g. `` `tag`[text](color: "red", size: "large")``
- [ ] Shortcut for applying inline-like parameters to blocks
  - e.g. ``> `callout`(type: info)``
- [ ] Nominal typing?
- [ ] Structural typing union & intersection?
- [ ] Tagged unions?
- [ ] Inheritance?
- [ ] Punctuation for smart quotes and en/em dashes

## Reference

- Content modelling
  - https://alistapart.com/article/content-modelling-a-master-skill/
  - https://unifiedjs.com/
- Data schemas
  - https://json-schema.org/draft/2020-12/json-schema-validation.html
- Markdown
  - https://commonmark.org/
  - https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax
  - https://www.reddit.com/wiki/markdown/
  - https://github.com/micromark/micromark
  - https://zslabs.com/articles/mdx-frontmatter-in-gatsby
  - https://www.fossil-scm.org/home/md_rules
- Asciidoctor
  - https://docs.asciidoctor.org/asciidoc/latest/macros/xref/
  - https://docs.asciidoctor.org/asciidoc/latest/lists/description/
- reStructuredText
  - https://www.sphinx-doc.org/en/master/usage/restructuredtext/roles.html
  - https://www.sphinx-doc.org/en/master/usage/restructuredtext/directives.html
- BBCode
  - https://www.phpbb.com/community/help/bbcode
- CMS
  - https://prismic.io/docs/technical-references
  - https://www.drupal.org/docs/creating-custom-modules/creating-custom-field-types-widgets-and-formatters/create-a-custom-field-type
  - https://www.drupal.org/docs/7/api/field-api/field-types-and-settings
  - https://www.contentful.com/developers/docs/concepts/data-model/
  - https://decapcms.org/docs/collection-types/
  - https://decapcms.org/docs/custom-widgets/

## Schema

`plant.md`

```md
# `organism`

## `uses: list of plant_use`

## `blurb: paragraph`

## `cultivation: list of block`
```

## Object

`tomato.md`

```md
# `plant`

## `name`

Tomato

## `uses`

- art
- cooking

## `cultivation`

Tomatoes are cultivated in hot climates.

### Pests

Beware of hornworm caterpillars.

> `callout`
>
> #### `type`
>
> info
>
> ---
>
> To see the bugs at night, use a [blacklight].
```

## View

`plant_card.md`

```md
# `block`

## `object: plant`

---

This is a $`object.name`.

## `for use in object.uses`

> Here is a use:
>
> > `use_card`
> >
> > ### `use`
> >
> > $`use`
```

## Builtin data types

- Boolean
- Number
- URL
- Email
- Link
- Tag
- Enum
- Text
- Author
- Group
- Color
- Geo point
- Geo shape

## Builtin rich text inlines

### HTML inlines

- Link
- Emphasis
- Strong
- Strong emphasis
- Subscript
- Superscript
- Code
- Time
- Abbreviation
- Highlight
- Strikethrough
- Math

### Custom inlines

- Footnote
- Color
- Mention
- Tag
- Emoji
- Spoiler

## Builtin rich text blocks

### HTML blocks

- Header
- Paragraph
- Unordered list
- Ordered list
- Definition list
- Code
- Media
- Quote
- Form
- Math
- Address
- Aside
- Figure
- Horizontal rule

### Custom blocks

- Link card
- Task list
- Callout
- Map
- Verse
- Spoiler

## Custom data types

- URLs to specific domains
- Text matching regexes

## Schemas

Schemas can contain basic conditional logic. And provide default values.

## Typing

All schemas, objects, and views are bound by static typing. This means they can all be fully checked for referential and schema integrity at design time.

## References

Rich text links and data links can reference self and other root and sub- objects.

When a data link type restriction is used, the type can match any linked root or sub- object.

Sub-object anchor IDs are auto-generated "slugs" (same as other slugs in Intertwine). Saved to the database to preserve history to avoid broken links.

## Headers

Content header levels will get translated to the view levels, and all view levels will get translated to proper HTML levels using page context.

## Collections

Both lists and maps have multiple representation options. The object's schema can specify a hint to the editor.

### Lists

- Paragraph separator
- Horizontal rule separator
- Ordered list
- Unordered list
- Table
- JSON

### Maps

- Header
- Table
- Defintiion list
- JSON

## Documentation

Each element of the schema should allow documentation, including example values.

## Aggregates

Some views show multiple objects -- this can be built with simple filters.

Also, some objects can be configured as singletons.

## Editing

- Syntax highlighting for Markdown features
- Proportional-width text, wrapped lines
- Editing styles match output (e.g. bold, headers, list indent), but syntax sitll visible
- Full workspace refactoring

## Review

Other authors can review schemas/objects/views before publishing. Content owners (like code owners) can be annotated into the folder hierarchy.

Multiple changes can be included in one review -- this allows making wide-reaching schema changes with confidence, because the changes can't be promoted while there are static type errors.

## Workspaces

Clone, delete, promote worksapces. This allows experimenting with invasive schema changes in isolation (from prod and other devs).

## Scheduling

Views/objects can go live at specific dates in the future.

import gleam/option.{type Option}
import lib_dataflow as dataflow
import lib_error
import lib_markup_dom as dom
import lib_markup_fragment as fragment
import lib_markup_scheduler as markup_scheduler

// The source exposes a context module whose Context contains the markup
// capabilities. Gleam has no structural intersection types, so the natural
// representation is the common MarkupContext value carried by fragments and
// custom builders. The accessors keep the browser capabilities explicit at
// call sites without introducing an untyped record or dynamic property
// lookup.
pub type Context =
  fragment.MarkupContext

pub type Markup =
  fragment.MarkupContext

pub type ScopedContext =
  fragment.ScopedContext

pub fn document(context: Context) -> dom.Document {
  fragment.markup_document(context)
}

pub fn scheduler(context: Context) -> markup_scheduler.Scheduler {
  fragment.markup_scheduler(context)
}

pub fn style_layer(context: Context) -> Option(dom.StyleLayer) {
  fragment.markup_style_layer(context)
}

pub fn scoped_markup(context: ScopedContext) -> Markup {
  fragment.scoped_markup(context)
}

pub fn scoped_document(context: ScopedContext) -> dom.Document {
  fragment.scoped_document(context)
}

pub fn scoped_scheduler(context: ScopedContext) -> markup_scheduler.Scheduler {
  fragment.scoped_scheduler(context)
}

pub fn scoped_dataflow(context: ScopedContext) -> dataflow.Context {
  fragment.scoped_dataflow(context)
}

pub fn scoped_effect(
  context: ScopedContext,
  callback: fn(value) -> Nil,
  computation: dataflow.Computation(value),
) -> Nil {
  fragment.scoped_effect(context, callback, computation)
}

pub fn scoped_effect_async(
  context: ScopedContext,
  callback: fn(value) -> lib_error.Async(Nil, reason),
  computation: dataflow.Computation(value),
) -> lib_error.Async(Nil, reason) {
  fragment.scoped_effect_async(context, callback, computation)
}

pub fn scoped_effect_async2(
  context: ScopedContext,
  callback: fn(first, second) -> lib_error.Async(Nil, reason),
  first: dataflow.Computation(first),
  second: dataflow.Computation(second),
) -> lib_error.Async(Nil, reason) {
  fragment.scoped_effect_async2(context, callback, first, second)
}

pub fn scoped_effect_async3(
  context: ScopedContext,
  callback: fn(first, second, third) -> lib_error.Async(Nil, reason),
  first: dataflow.Computation(first),
  second: dataflow.Computation(second),
  third: dataflow.Computation(third),
) -> lib_error.Async(Nil, reason) {
  fragment.scoped_effect_async3(context, callback, first, second, third)
}

pub fn scoped_defer(context: ScopedContext, callback: fn() -> Nil) -> Nil {
  fragment.scoped_defer(context, callback)
}

// This callback form is the Gleam equivalent of awaiting the source
// scheduler. The scheduler remains the explicit asynchronous runtime seam.
pub fn wait(context: Context, callback: fn() -> Nil) -> Nil {
  markup_scheduler.wait(scheduler(context), callback)
}

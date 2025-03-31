#![feature(imported_main)]
#![feature(trait_alias)]

pub mod command;
pub mod context;
pub mod help;
pub mod main_;
pub mod parse;
pub mod run;

pub use main_::main;

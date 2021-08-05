#![warn(missing_docs)]
#![allow(dead_code)]

//! Typed Erlang - typed_erlc
//! A fusion of Erlang compiler erlc (lib/compile in OTP repository) and compile-time type checker,
//! somewhat like Dialyzer (lib/dialyzer in OTP repository).

pub mod syntaxtree;
pub mod project;
pub mod erl_module;
pub mod erl_error;
pub mod stage;
pub mod typing;
mod funarity;
mod source_loc;

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate toml;
extern crate serde;
extern crate serde_derive;
// extern crate thiserror;
extern crate glob;
extern crate alphabet;
extern crate lazy_static;
// extern crate enum_as_inner;
extern crate function_name;

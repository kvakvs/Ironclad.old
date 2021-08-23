#![warn(missing_docs)]
#![allow(dead_code)]

//! Typed Erlang - typed_erlc
//! A fusion of Erlang compiler erlc (lib/compile in OTP repository) and compile-time type checker,
//! somewhat like Dialyzer (lib/dialyzer in OTP repository).

pub mod project;
pub mod erl_error;
pub mod stage;
pub mod typing;
pub mod mfarity;
pub mod source_loc;
pub mod display;
pub mod erlang;
pub mod preprocessor;
pub mod core_erlang;
pub mod ast_tree;
pub mod literal;

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate toml;
extern crate serde;
extern crate serde_derive;
extern crate glob;
extern crate alphabet;
extern crate lazy_static;
extern crate function_name;
#[macro_use]
extern crate derivative;

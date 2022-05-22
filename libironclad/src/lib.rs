#![warn(missing_docs)]
#![allow(dead_code)]

//! # Ironclad parser and type analyzer + compiler
//!
//! A fusion of Erlang erlc (lib/compile in OTP repository) and compile-time type checker,
//! somewhat like Dialyzer (lib/dialyzer in OTP repository).

pub mod project;
pub mod erl_error;
pub mod stage;
pub mod erlang;
pub mod core_erlang;

extern crate toml;
extern crate serde;
extern crate serde_derive;
extern crate glob;
extern crate alphabet;
extern crate lazy_static;
extern crate function_name;
#[macro_use]
extern crate derivative;

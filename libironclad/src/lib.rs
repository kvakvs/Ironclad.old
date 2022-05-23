#![warn(missing_docs)]
#![allow(dead_code)]

//! # Ironclad parser and type analyzer + compiler
//!
//! A fusion of Erlang erlc (lib/compile in OTP repository) and compile-time type checker,
//! somewhat like Dialyzer (lib/dialyzer in OTP repository).

pub mod core_erlang;
pub mod project;
pub mod stage;

extern crate alphabet;
extern crate function_name;
extern crate glob;
extern crate lazy_static;
extern crate serde;
extern crate serde_derive;
extern crate toml;
// extern crate derivative;

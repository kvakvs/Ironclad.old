#![warn(missing_docs)]
#![allow(dead_code)]

//! # Ironclad parser and type analyzer + compiler
//!
//! A fusion of Erlang erlc (lib/compile in OTP repository) and compile-time type checker,
//! somewhat like Dialyzer (lib/dialyzer in OTP repository).

extern crate alphabet;
extern crate function_name;
extern crate glob;
extern crate lazy_static;

pub mod stage;

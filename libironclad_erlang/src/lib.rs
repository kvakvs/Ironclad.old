//! Crate handling Erlang syntax and typing
#![warn(missing_docs)]
extern crate core;
extern crate derivative;
extern crate lazy_static;
extern crate libironclad_error;
extern crate libironclad_util;
extern crate nom;
extern crate num;
extern crate num_bigint;

pub mod erl_syntax;
pub mod literal;
pub mod typing;

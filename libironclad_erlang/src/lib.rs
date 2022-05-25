//! Crate handling Erlang syntax and typing
#![warn(missing_docs)]
extern crate derivative;
extern crate lazy_static;
extern crate libironclad_error;
extern crate libironclad_util;
extern crate nom;

pub mod literal;
pub mod syntax_tree;
pub mod typing;

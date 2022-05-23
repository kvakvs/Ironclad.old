//! Printing for Erlang errors

use crate::syntax_tree::erl_error::ErlError;
use std::fmt::Formatter;

impl std::fmt::Display for ErlError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?}", self)
  }
}

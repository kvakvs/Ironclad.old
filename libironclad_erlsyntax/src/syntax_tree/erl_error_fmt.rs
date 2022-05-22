//! Printing for Erlang errors

use std::fmt::Formatter;
use crate::syntax_tree::erl_error::ErlError;

impl std::fmt::Display for ErlError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self.err_type {}
  }
}
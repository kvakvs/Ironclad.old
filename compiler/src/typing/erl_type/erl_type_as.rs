//! Conversions into sub-nodes

use std::ops::Deref;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_type::FnType;

impl ErlType {
  pub fn as_fn_def(&self) -> &FnType {
    match self {
      ErlType::Fn(t) => t.deref(),
      _ => panic!("ErlType expected to be a fn type, but got {}", self)
    }
  }
}
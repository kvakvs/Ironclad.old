//! Conversions into sub-nodes

use crate::typing::erl_type::ErlType;
use crate::typing::fn_type::FnType;
use crate::typing::type_union::TypeUnion;
use std::ops::Deref;

impl ErlType {
  /// Access ErlType as a fumction type
  pub fn as_fn_type(&self) -> &FnType {
    match self {
      ErlType::Fn(t) => t.deref(),
      _ => panic!("ErlType expected to be a fn type, but got {}", self),
    }
  }

  /// Access ErlType as an union
  pub fn as_union(&self) -> &TypeUnion {
    match self {
      ErlType::Union(tu) => tu,
      _ => panic!("ErlType expected to be an union, but got {}", self),
    }
  }
}

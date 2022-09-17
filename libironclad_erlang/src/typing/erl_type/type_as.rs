//! Conversions into sub-nodes

use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::TypeImpl;
use crate::typing::fn_type::FnType;
use crate::typing::type_union::TypeUnion;
use std::ops::Deref;

impl TypeImpl {
  /// Access ErlType as a fumction type
  pub fn as_fn_type(&self) -> &FnType {
    match &self.kind {
      TypeKind::Fn(t) => t.deref(),
      _ => panic!("ErlType expected to be a fn type, but got {}", self),
    }
  }

  /// Access ErlType as an union
  pub(crate) fn as_union(&self) -> &TypeUnion {
    match &self.kind {
      TypeKind::Union(tu) => tu,
      _ => panic!("ErlType expected to be an union, but got {}", self),
    }
  }
}

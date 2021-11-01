//! Union type (a flat list of multiple types) support

use std::sync::Arc;
use crate::typing::erl_type::ErlType;

/// Contains multiple types
#[derive(Debug, Eq, PartialEq)]
pub struct TypeUnion {
  types: Vec<Arc<ErlType>>,
  // lists: Vec<Arc<ErlType>>,
  // tuples: Vec<Arc<ErlType>>,
  // records: Vec<Arc<ErlType>>,
  // maps: Vec<Arc<ErlType>>,
  // binaries: Vec<Arc<ErlType>>,
}

impl TypeUnion {
  /// Create a type union from a vec of types
  pub fn new(types: Vec<Arc<ErlType>>) -> Self {
    Self { types }
  }
}

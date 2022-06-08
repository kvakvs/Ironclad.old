//! Define type for a map

use crate::typing::erl_type::ErlType;
use std::sync::Arc;

/// Defines a type for a member of map value `Type1 => Type2` a part of map type
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MapMemberType {
  /// The type of a key
  pub key: Arc<ErlType>,
  /// The corresponding value type
  pub value: Arc<ErlType>,
}
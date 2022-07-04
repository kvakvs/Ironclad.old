//! Define type for a map

use crate::typing::erl_type::ErlType;
use std::fmt::{Display, Formatter};

/// Defines a type for a member of map value `Type1 => Type2` a part of map type
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MapMemberType {
  /// The type of a key
  pub key: ErlType,
  /// The corresponding value type
  pub value: ErlType,
}

impl Display for MapMemberType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} => {}", self.key, self.value)
  }
}

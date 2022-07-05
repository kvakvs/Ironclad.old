//! Defines a pair of record field name and its type

use crate::typing::erl_type::ErlType;
use std::fmt::Formatter;

/// Record field is a pair of field name and type
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RecordFieldType {
  /// Record field name atom, stored as string
  pub name: String,
  /// Record field typespec
  pub ty: ErlType,
}

impl RecordFieldType {
  /// Create a record field type, or a pin
  pub fn new(name: String, ty: ErlType) -> Self {
    Self { name, ty }
  }
}

impl std::fmt::Display for RecordFieldType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} :: {}", self.name, self.ty)
  }
}

//! Defines a pair of record field name and its type

use crate::typing::erl_type::ErlType;
use std::fmt::Formatter;
use std::sync::Arc;

/// Record field is a pair of field name and type
#[derive(Debug, Eq, PartialEq)]
pub struct RecordFieldType {
  /// Record field name atom, stored as string
  pub name: String,
  /// Record field typespec
  pub ty: Arc<ErlType>,
}

impl std::fmt::Display for RecordFieldType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} :: {}", self.name, self.ty)
  }
}

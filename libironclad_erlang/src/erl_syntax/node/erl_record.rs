//! Record definition: Fields

use crate::erl_syntax::erl_ast::AstNode;
use crate::typing::erl_type::ErlType;
use std::fmt::Formatter;
use std::sync::Arc;

/// Record field has a name, optional initializer, and optional type ascription. Defined with
/// `-record(name, {fields, ...}).` module attributes.
#[derive(Clone, Debug)]
pub struct RecordField {
  /// Record field name atom, stored as string
  pub field_tag: String,
  /// Optional initializer for construction of the record
  pub initializer: Option<AstNode>,
  /// Optional type ascription
  pub type_ascription: Option<Arc<ErlType>>,
}

impl std::fmt::Display for RecordField {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.field_tag)?;
    if let Some(init) = &self.initializer {
      write!(f, " = {}", init)?;
    }
    if let Some(ascr) = &self.type_ascription {
      write!(f, " :: {}", ascr)?;
    }
    Ok(())
  }
}

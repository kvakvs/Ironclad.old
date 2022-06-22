//! Record definition container

use crate::erl_syntax::node::erl_record::RecordField;
use libironclad_util::pretty::Pretty;
use std::fmt::Formatter;

/// Describes a record defined in a module
#[derive(Debug)]
pub struct RecordDefinition {
  /// The record tag
  pub tag: String,
  /// The fields
  pub fields: Vec<RecordField>,
  // /// The synthesized type
  // pub ty: Arc<ErlType>,
}

impl std::fmt::Display for RecordDefinition {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{tag}{{", tag = self.tag)?;
    Pretty::display_comma_separated(self.fields.iter(), f)?;
    write!(f, "}}")
  }
}

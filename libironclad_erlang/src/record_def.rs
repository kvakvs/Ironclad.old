//! Record definition container

use crate::erl_syntax::node::erl_record::RecordField;

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

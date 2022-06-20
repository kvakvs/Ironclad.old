//! Module attributes collection

use crate::erl_syntax::erl_ast::AstNode;
use std::sync::RwLock;

/// Storage for one `- <NAME> ( <EXPR> )` module attribute
#[derive(Debug)]
pub struct ModuleAttribute {
  /// The tag (multiple attributes per tag are allowed)
  pub tag: String,
  /// The contents
  pub expr: AstNode,
}

/// Module attributes are non-unique and grouped together by name.
#[derive(Debug)]
pub struct ModuleAttributes {
  pub collection: RwLock<Vec<ModuleAttribute>>,
}

impl Default for ModuleAttributes {
  fn default() -> Self {
    Self { collection: Default::default() }
  }
}

impl ModuleAttributes {
  /// Retrieve collection length
  pub fn len(&self) -> usize {
    if let Ok(r_collection) = self.collection.read() {
      r_collection.len()
    } else {
      panic!("Can't lock module attributes named group for length check")
    }
  }
}

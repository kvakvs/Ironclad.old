//! Module attributes collection

use crate::erl_syntax::erl_ast::AstNode;
use std::sync::RwLock;

/// Storage for one `- <NAME> ( <EXPR> )` module attribute
#[derive(Debug, Clone)]
pub struct ModuleAttribute {
  /// The tag (multiple attributes per tag are allowed)
  pub tag: String,
  /// The contents
  pub expr: Option<AstNode>,
}

impl ModuleAttribute {
  /// Create a new
  pub fn new(tag: String, expr: Option<AstNode>) -> Self {
    Self { tag, expr }
  }
}

/// Module attributes are non-unique and grouped together by name.
#[derive(Debug)]
pub struct ModuleAttributes {
  /// Array of module attributes sharing same name
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

  /// Retrieve one element
  pub fn get(&self, index: usize) -> ModuleAttribute {
    if let Ok(r_collection) = self.collection.read() {
      r_collection[index].clone()
    } else {
      panic!("Can't lock module attributes named group to retrieve an attribute")
    }
  }

  /// Append an element
  pub fn push(&self, tag: String, expr: Option<AstNode>) {
    if let Ok(mut w_collection) = self.collection.write() {
      w_collection.push(ModuleAttribute::new(tag, expr))
    } else {
      panic!("Can't lock module attributes named group to insert")
    }
  }
}

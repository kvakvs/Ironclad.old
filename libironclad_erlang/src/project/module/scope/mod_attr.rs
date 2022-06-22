//! Module attributes collection

use crate::erl_syntax::erl_ast::AstNode;
use libironclad_util::pretty::Pretty;
use std::fmt::Formatter;
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

impl std::fmt::Display for ModuleAttributes {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Ok(r_collection) = self.collection.read() {
      Pretty::display_square_list(r_collection.iter(), f)
    } else {
      panic!("Can't lock ModuleAttributes for printing")
    }
  }
}

impl std::fmt::Display for ModuleAttribute {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.expr {
      Some(expr1) => write!(f, "{tag}={expr}", tag = self.tag, expr = expr1),
      None => write!(f, "{tag}", tag = self.tag),
    }
  }
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
      panic!("Can't lock ModuleAttributes for length check")
    }
  }

  /// Retrieve one element
  pub fn get(&self, index: usize) -> ModuleAttribute {
    if let Ok(r_collection) = self.collection.read() {
      r_collection[index].clone()
    } else {
      panic!("Can't lock ModuleAttributes to retrieve an attribute")
    }
  }

  /// Append an element
  pub fn push(&self, tag: String, expr: Option<AstNode>) {
    if let Ok(mut w_collection) = self.collection.write() {
      w_collection.push(ModuleAttribute::new(tag, expr))
    } else {
      panic!("Can't lock ModuleAttributes to insert")
    }
  }
}

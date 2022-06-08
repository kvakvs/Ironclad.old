//! Access to sub-values in ErlAst

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, ErlAstType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_fn_def::ErlFnDef;
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::literal::Literal;
use crate::typing::erl_type::ErlType;
use libironclad_util::mfarity::MFArity;
use std::ops::Deref;
use std::sync::Arc;

impl AstNodeImpl {
  /// Unwrap self as new function
  pub fn as_fn_def(&self) -> &ErlFnDef {
    match &self.content {
      ErlAstType::FnDef(func_def) => func_def,
      _ => panic!("Expected FnDef AST node, but got {}", self),
    }
  }

  /// Unwrap self as function spec
  pub fn as_fn_spec(&self) -> Arc<ErlType> {
    match &self.content {
      ErlAstType::FnSpec { spec, .. } => spec.clone(),
      _ => panic!("Expected FnSpec AST node, but got {}", self),
    }
  }

  /// Unwrap self as erltype
  pub fn as_type(&self) -> Arc<ErlType> {
    match &self.content {
      ErlAstType::Type { ty, .. } => ty.clone(),
      _ => panic!("Expected Type AST node, but got {}", self),
    }
  }

  /// Unwrap self as binary operation expr
  pub fn as_binop(&self) -> &ErlBinaryOperatorExpr {
    match &self.content {
      ErlAstType::BinaryOp { expr, .. } => expr,
      _ => panic!("Expected BinOp AST node, but got {}", self),
    }
  }

  /// Unwrap self as an atom (return string slice or `panic`)
  pub fn as_atom(&self) -> &str {
    match &self.content {
      ErlAstType::Lit { value, .. } => match value.deref() {
        Literal::Atom(s) => s,
        _ => panic!("Expected Lit(Atom()) AST node, but got {}", self),
      },
      _ => panic!("Expected Lit(Atom()) AST node, but got {}", self),
    }
  }

  /// Unwrap a preprocessor node
  pub fn as_preprocessor(&self) -> &PreprocessorNodeType {
    match &self.content {
      ErlAstType::Preprocessor(pp) => pp,
      _ => panic!("Expected Preprocessor() AST node, but got {}", self),
    }
  }

  /// Unwrap an `-export` attr and return contents
  pub fn as_export_attr(&self) -> &Vec<MFArity> {
    match &self.content {
      ErlAstType::ExportAttr { exports } => exports,
      _ => panic!("Expected ExportAttr() AST node, but got {}", self),
    }
  }

  /// Unwrap an `-export_types` attr and return contents
  pub fn as_export_types_attr(&self) -> &Vec<MFArity> {
    match &self.content {
      ErlAstType::ExportTypesAttr { exports } => exports,
      _ => panic!("Expected ExportTypesAttr() AST node, but got {}", self),
    }
  }

  /// Unwrap an `-include` attr and return path
  pub fn as_preprocessor_include(&self) -> &str {
    match self.as_preprocessor() {
      PreprocessorNodeType::Include(path) => path.as_str(),
      _ => panic!("Expected Preprocessor::Include() AST node, but got {}", self),
    }
  }

  /// Unwrap an `-define` attr and return path
  pub fn as_preprocessor_define(&self) -> (&str, &[String], &str) {
    match self.as_preprocessor() {
      PreprocessorNodeType::Define { name, args, body } => (name.as_str(), args, body.as_str()),
      _ => panic!("Expected Preprocessor::Define() AST node, but got {}", self),
    }
  }

  /// Unwrap an `-import` attr and return (import_name, imported_mfas)
  pub fn as_import_attr(&self) -> (&str, &Vec<MFArity>) {
    match &self.content {
      ErlAstType::ImportAttr { import_from, imports } => (import_from.as_str(), imports),
      _ => panic!("Expected Import() AST node, but got {}", self),
    }
  }

  /// Unwrap a `ModuleRoot` node. Returns name and child nodes vector. Returns children first
  /// level only, for flat list of everything use `.children()` call.
  pub fn as_module(&self) -> (&str, &Vec<AstNode>) {
    match &self.content {
      ErlAstType::ModuleRoot { name, forms } => (name.as_str(), forms),
      _ => panic!("Expected ModuleRoot() AST node, but got {}", self),
    }
  }
}

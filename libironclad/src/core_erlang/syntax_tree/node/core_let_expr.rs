//! Haskell-style `let x = Value in Expr` node
#![cfg(coreast)]
use std::sync::Arc;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::core_var::Var;
use libironclad_util::source_loc::SourceLoc;

/// Represents Haskell-style `let x = Value in Expr` node, used in Core Erlang to create scopes
#[derive(Debug)]
#[cfg(coreast)]
pub struct LetExpr {
  /// Source file pointer
  location: SourceLoc,
  /// The variable name assigned in let..in
  pub var: Vec<Var>,

  /// Value (type is in it), must return a vector of as many values, as `Self::var` has variables.
  pub value: Arc<CoreAst>,

  /// Let x=y in <body> (type is in it, and becomes type of Expr::Let)
  pub in_expr: Arc<CoreAst>,
}

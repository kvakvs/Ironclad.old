//! Defines structs for AST nodes representing binary operators (A + B)
#![cfg(coreast)]
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::core_op::{CoreBinaryOp};
use crate::erl_error::{ErlError, ErlResult};
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;
use crate::typing::type_error::TypeError;

/// Binary operator is a code structure `Expr <operator> Expr`
#[derive(Debug)]
#[cfg(coreast)]
pub struct BinaryOperatorExpr {
  /// Left operand
  pub left: Arc<CoreAst>,
  /// Right operand
  pub right: Arc<CoreAst>,
  /// The operator
  pub operator: CoreBinaryOp,
}

#[cfg(coreast)]
impl BinaryOperatorExpr {
}

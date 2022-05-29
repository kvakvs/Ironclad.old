//! AST node-type checks

use crate::erl_syntax::erl_ast::{ErlAst, ErlAstType};
use crate::erl_syntax::erl_op::ErlBinaryOp;
use crate::literal::Literal;
use std::ops::Deref;

impl ErlAst {
  /// Checks whether an ErlAst node is a function definition
  pub fn is_fn_def(&self) -> bool {
    matches!(&self.content, ErlAstType::FnDef(_))
  }

  /// Checks whether an ErlAst node is a function spec
  pub fn is_fn_spec(&self) -> bool {
    matches!(&self.content, ErlAstType::FnSpec { .. })
  }

  /// Checks whether an ErlAst node is an Erlang Type
  pub fn is_type(&self) -> bool {
    matches!(self.content, ErlAstType::Type { .. })
  }

  /// Checks whether an ErlAst node is an Erlang Type
  pub fn is_atom(&self) -> bool {
    match &self.content {
      ErlAstType::Lit { value, .. } => matches!(value.deref(), Literal::Atom(_)),
      _ => false,
    }
  }

  /// Checks whether an ErlAst node is a Binary Op of given kind
  pub fn is_binop(&self, op: ErlBinaryOp) -> bool {
    matches!(&self.content, ErlAstType::BinaryOp {expr, ..} if expr.operator == op)
  }

  /// Checks whether an ErlAst node is a Binary Expression
  pub fn is_binary(&self) -> bool {
    matches!(&self.content, ErlAstType::BinaryExpr { .. })
  }

  /// Checks whether an ErlAst node is a Function Application (a call)
  pub fn is_application(&self) -> bool {
    matches!(&self.content, ErlAstType::Apply(_))
  }
}

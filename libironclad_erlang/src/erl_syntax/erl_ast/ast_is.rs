//! AST node-type checks

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_op::ErlBinaryOp;
use crate::literal::Literal;
use std::ops::Deref;

impl AstNodeImpl {
  /// Checks whether an ErlAst node is a function definition
  pub fn is_fn_def(&self) -> bool {
    matches!(&self.content, AstNodeType::FnDef(_))
  }

  /// Checks whether an ErlAst node is a tuple
  pub fn is_tuple(&self) -> bool {
    matches!(&self.content, AstNodeType::Tuple { .. })
  }

  /// Checks whether an AST node is an `Empty`
  pub fn is_empty_ast_node(&self) -> bool {
    matches!(&self.content, AstNodeType::Empty { .. })
  }

  /// Checks whether an ErlAst node is an Erlang Type
  pub fn is_type(&self) -> bool {
    matches!(self.content, AstNodeType::Type { .. })
  }

  /// Checks whether an ErlAst node is an atom (any atom)
  pub fn is_atom(&self) -> bool {
    match &self.content {
      AstNodeType::Lit { value, .. } => matches!(value.deref(), Literal::Atom(_)),
      _ => false,
    }
  }

  /// Checks whether an ErlAst node is an atom and compare the atom string name
  pub fn is_atom_of(&self, atom: &str) -> bool {
    match &self.content {
      AstNodeType::Lit { value, .. } => matches!(value.deref(), Literal::Atom(a) if a == atom),
      _ => false,
    }
  }

  /// Checks whether an ErlAst node is a Binary Op of given kind
  pub fn is_binop(&self, op: ErlBinaryOp) -> bool {
    matches!(&self.content, AstNodeType::BinaryOp {binop_expr, ..} if binop_expr.operator == op)
  }

  /// Checks whether an ErlAst node is a Binary Expression
  pub fn is_binary(&self) -> bool {
    match &self.content {
      AstNodeType::BinaryExpr { .. } => true,
      AstNodeType::Lit { value } => value.is_binary_lit(),
      _ => false,
    }
  }

  /// Checks whether an ErlAst node is a Binary Comprehension
  pub fn is_binary_comprehension(&self) -> bool {
    matches!(&self.content, AstNodeType::BinaryComprehension { .. })
  }

  /// Checks whether an ErlAst node is a Record Field Accessor
  pub fn is_record_field(&self) -> bool {
    matches!(&self.content, AstNodeType::RecordField { .. })
  }

  /// Checks whether an ErlAst node is a Record Builder
  pub fn is_record_builder(&self) -> bool {
    matches!(&self.content, AstNodeType::RecordBuilder { .. })
  }

  /// Checks whether an ErlAst node is a Function Application (a call)
  pub fn is_application(&self) -> bool {
    matches!(&self.content, AstNodeType::Apply(_))
  }

  /// Checks whether an ErlAst node is a Case Expression
  pub fn is_case_expr(&self) -> bool {
    matches!(&self.content, AstNodeType::CaseExpr { .. })
  }
}

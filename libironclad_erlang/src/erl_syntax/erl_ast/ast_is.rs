//! AST node-type checks

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_op::ErlBinaryOp;
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::literal::Literal;
use std::ops::Deref;

impl AstNodeImpl {
  /// Checks whether an ErlAst node is a function definition
  pub fn is_fn_def(&self) -> bool {
    matches!(&self.content, AstNodeType::FnDef(_))
  }

  /// Checks whether an AST node is an `Empty`
  pub fn is_empty_ast_node(&self) -> bool {
    matches!(&self.content, AstNodeType::Empty { .. })
  }

  /// Checks whether an ErlAst node is a function spec
  pub fn is_fn_spec(&self) -> bool {
    matches!(&self.content, AstNodeType::FnSpec { .. })
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
    matches!(&self.content, AstNodeType::BinaryOp {expr, ..} if expr.operator == op)
  }

  /// Checks whether an ErlAst node is a Binary Expression
  pub fn is_binary(&self) -> bool {
    matches!(&self.content, AstNodeType::BinaryExpr { .. })
  }

  /// Checks whether an ErlAst node is a Function Application (a call)
  pub fn is_application(&self) -> bool {
    matches!(&self.content, AstNodeType::Apply(_))
  }

  /// Checks whether an ErlAst node is a Preprocessor Warning with given text
  pub fn is_preprocessor_warning(&self, text: &str) -> bool {
    let pp_node = self.as_preprocessor();
    if let PreprocessorNodeType::Warning(s) = pp_node {
      s == text
    } else {
      panic!("Preprocessor::Warning node was expected, but found {}", pp_node)
    }
  }
}

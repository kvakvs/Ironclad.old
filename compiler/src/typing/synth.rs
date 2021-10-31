//! Synthesize a type from AST node
use std::sync::Arc;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::typing::erl_type::ErlType;

/// Hosts code to synthesize ErlTypes from AST
pub struct TypeBuilder {}

impl TypeBuilder {
  /// From core AST subtree, create a type which we believe it will have, narrowest possible.
  /// It will be further narrowed later, if we don't happen to know at this moment.
  pub fn synthesize_from_core(node: &CoreAst) -> Arc<ErlType> {
    match node {
      CoreAst::Empty => unreachable!("Should not be synthesizing type from empty AST nodes"),
      CoreAst::Module { .. } => unreachable!("Should not be synthesizing type from module node"),
      CoreAst::Attributes(_) => unreachable!("Should not be synthesizing type from module attrs"),
      CoreAst::ModuleFuns(_) => unreachable!("Should not be synthesizing type from module functions section"),
      // CoreAst::FnDef(_) => {}
      // CoreAst::FnRef { .. } => {}
      // CoreAst::Case(_) => {}
      // CoreAst::Let(_) => {}
      // CoreAst::Apply(_) => {}
      // CoreAst::Call(_) => {}
      // CoreAst::PrimOp { .. } => {}
      // CoreAst::Var(_) => {}
      CoreAst::Lit { value, .. } => ErlType::new_singleton(value),
      CoreAst::BinOp { op, .. } => op.synthesize_type(),
      // CoreAst::UnOp { .. } => {}
      CoreAst::List { .. } => ErlType::AnyList.into(),
      CoreAst::Tuple { .. } => ErlType::AnyTuple.into(),
      other => unimplemented!("Don't know how to synthesize type from {}", other),
    }
  }
}

//! Contains code to build Core AST from Erlang AST
use function_name::named;
use std::sync::Arc;
use std::ops::Deref;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::typing::typevar::TypeVar;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::core_erlang::syntax_tree::node::fn_def::FnDef;
use std::iter;
use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::core_erlang::syntax_tree::node::case::Case;

mod fn_def;

/// Dummy struct containing building code for Core AST from Erlang AST
pub struct CoreAstBuilder {}

impl CoreAstBuilder {
  /// Rebuild Core Erlang AST from Erlang AST
  #[named]
  pub fn build(ast: Arc<ErlAst>) -> Arc<CoreAst> {
    match ast.deref() {
      ErlAst::Empty => CoreAst::Empty.into(),
      // ErlAst::ModuleForms(_) => {}
      // ErlAst::ModuleAttr { .. } => {}
      ErlAst::FnDef { .. } => Self::create_from_fndef(ast),
      // ErlAst::CClause(_, _) => {}
      // ErlAst::MFA { .. } => {}
      // ErlAst::Var(_, _) => {}
      // ErlAst::Apply(_, _) => {}
      // ErlAst::Case(_, _) => {}
      // ErlAst::Lit(_, _) => {}
      // ErlAst::BinaryOp(_, _) => {}
      // ErlAst::UnaryOp(_, _) => {}
      // ErlAst::List { .. } => {}
      // ErlAst::Tuple { .. } => {}

      other => unimplemented!("{}: Don't know how to convert ErlAst {} into CoreAst",
                              function_name!(), other)
    }
  }
}
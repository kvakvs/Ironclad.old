//! Contains code to build Core AST from Erlang AST
use function_name::named;
use std::sync::Arc;
use std::ops::Deref;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::typing::typevar::TypeVar;
use crate::erlang::syntax_tree::node::fn_def::FnDef;

/// Dummy struct containing building code for Core AST from Erlang AST
pub struct CoreAstBuilder {}

impl CoreAstBuilder {
  /// Rebuild Core Erlang AST from Erlang AST
  #[named]
  pub fn create_from_erl(ast: Arc<ErlAst>) -> Arc<CoreAst> {
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

  /// Given a FnDef, produce a CoreAst equivalent new function definition with an optional nested
  /// case for multiple clauses
  #[named]
  fn create_from_fndef(ast: Arc<ErlAst>) -> Arc<CoreAst> {
    if let ErlAst::FnDef {
      location,
      funarity, ret_ty,
      fn_def
    } = ast.deref() {
      // return CoreAst::FnDef { location, core_fn_def }
    }

    panic!("{}: Not a ErlAst::FnDef - got {}", function_name!(), ast)
  }
}
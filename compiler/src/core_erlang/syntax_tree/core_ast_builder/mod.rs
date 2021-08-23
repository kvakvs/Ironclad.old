//! Contains code to build Core AST from Erlang AST
use function_name::named;
use std::sync::Arc;
use std::ops::Deref;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::core_erlang::syntax_tree::node::expression::BinaryOperatorExpr;
use crate::typing::typevar::TypeVar;
use crate::core_erlang::syntax_tree::node::var::Var;
use crate::core_erlang::syntax_tree::node::apply::Apply;

mod fn_def;

/// Dummy struct containing building code for Core AST from Erlang AST
pub struct CoreAstBuilder {}

impl CoreAstBuilder {
  /// Rebuild Core Erlang AST from Erlang AST
  #[named]
  pub fn build(ast: &Arc<ErlAst>) -> Arc<CoreAst> {
    match ast.deref() {
      ErlAst::Empty => CoreAst::Empty.into(),
      ErlAst::ModuleAttr { name, .. } => CoreAst::Module {
        name: name.clone(),
        exports: vec![],
      }.into(),
      ErlAst::ModuleForms(forms) => {
        let fndefs = forms.iter().map(Self::build).collect();
        return CoreAst::FunctionDefs(fndefs).into();
      }
      ErlAst::FnDef { .. } => Self::create_from_fndef(ast),
      // ErlAst::CClause(_, _) => {}
      // ErlAst::MFA { .. } => {}
      ErlAst::Var(erl_var) => {
        let core_var = Var {
          location: erl_var.location.clone(),
          name: Some(erl_var.name.clone()),
          ty: TypeVar::new(),
        };
        CoreAst::Var(core_var).into()
      }
      ErlAst::Apply(app) => {
        let core_app = Apply {
          location: app.location.clone(),
          target: Self::build(&app.expr),
          args: app.args.iter().map(Self::build).collect(),
          ret_ty: TypeVar::new()
        };
        CoreAst::Apply(core_app).into()
      }
      // ErlAst::Case(_, _) => {}
      // ErlAst::Lit(_, _) => {}
      ErlAst::BinaryOp(loc, binop) => {
        CoreAst::BinOp {
          location: loc.clone(),
          op: BinaryOperatorExpr {
            left: Self::build(&binop.left),
            right: Self::build(&binop.right),
            operator: binop.operator.into(),
            ty: TypeVar::new(),
          },
        }.into()
      }
      // ErlAst::UnaryOp(_, _) => {}
      // ErlAst::List { .. } => {}
      // ErlAst::Tuple { .. } => {}

      other => unimplemented!("{}: Don't know how to convert ErlAst {:?} into CoreAst",
                              function_name!(), other)
    }
  }
}
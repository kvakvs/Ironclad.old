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
use crate::project::module::Module;

mod fn_def;

/// Dummy struct containing building code for Core AST from Erlang AST
pub struct CoreAstBuilder {}

impl CoreAstBuilder {
  /// Rebuild Core Erlang AST from Erlang AST
  #[named]
  pub fn build(env: &Module, ast: &Arc<ErlAst>) -> Arc<CoreAst> {
    match ast.deref() {
      ErlAst::Empty => CoreAst::Empty.into(),
      ErlAst::ModuleAttr { name, .. } => CoreAst::Module {
        name: name.clone(),
        exports: vec![],
      }.into(),
      ErlAst::ModuleForms(forms) => {
        let fndefs = forms.iter()
            .map(|each_form| Self::build(env, each_form))
            .collect();
        CoreAst::ModuleFuns(fndefs).into()
      }
      ErlAst::FnDef { .. } => Self::create_from_fndef(env, ast),
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
          target: Self::build(env, &app.expr),
          args: app.args.iter()
              .map(|each_arg| Self::build(env, each_arg))
              .collect(),
          ret_ty: TypeVar::new()
        };
        CoreAst::Apply(core_app).into()
      }
      // ErlAst::Case(_, _) => {}
      ErlAst::Lit(loc, lit) => {
        CoreAst::Lit {
          location: loc.clone(),
          value: lit.clone(),
          lit_type: lit.get_type()
        }.into()
      }
      ErlAst::BinaryOp(loc, binop) => {
        CoreAst::BinOp {
          location: loc.clone(),
          op: BinaryOperatorExpr {
            left: Self::build(env, &binop.left),
            right: Self::build(env, &binop.right),
            operator: binop.operator.into(),
            ty: TypeVar::new(),
          },
        }.into()
      }
      // ErlAst::UnaryOp(_, _) => {}
      ErlAst::List { location, elements, tail } => {
        CoreAst::List {
          location: location.clone(),
          elements: elements.iter()
              .map(|each_el| Self::build(env, each_el))
              .collect(),
          tail: tail.as_ref().map(|t| Self::build(env, t))
        }.into()
      }
      // ErlAst::Tuple { .. } => {}

      other => unimplemented!("{}: Don't know how to convert ErlAst {:?} into CoreAst",
                              function_name!(), other)
    }
  }
}
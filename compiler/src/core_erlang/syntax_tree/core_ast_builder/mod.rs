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
use crate::erlang::syntax_tree::node::erl_apply::ErlApply;
use crate::erlang::syntax_tree::node::erl_var::ErlVar;
use crate::source_loc::SourceLoc;
use crate::literal::Literal;
use crate::erlang::syntax_tree::node::erl_expression::ErlBinaryOperatorExpr;

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
      ErlAst::Var(erl_var) => CoreAstBuilder::core_from_var(erl_var),
      ErlAst::Apply(app) => CoreAstBuilder::core_from_apply(env, &app),
      // ErlAst::Case(_, _) => {}
      ErlAst::Lit { location: loc, value: lit } => CoreAstBuilder::core_from_literal(loc, lit),
      ErlAst::BinaryOp(loc, binop) => {
        CoreAstBuilder::core_from_binaryop(env, loc, &binop)
      }
      // ErlAst::UnaryOp(_, _) => {}
      ErlAst::List { location, elements, tail } => {
        CoreAstBuilder::core_from_list(env, location, elements, tail)
      }
      // ErlAst::Tuple { .. } => {}

      other => unimplemented!("{}: Don't know how to convert ErlAst {:?} into CoreAst",
                              function_name!(), other)
    }
  }

  /// From Erl List rewrap a Core List
  fn core_from_list(env: &Module, location: &SourceLoc,
                    elements: &Vec<Arc<ErlAst>>,
                    tail: &Option<Arc<ErlAst>>) -> Arc<CoreAst> {
    CoreAst::List {
      location: location.clone(),
      elements: elements.iter()
          .map(|each_el| Self::build(env, each_el))
          .collect(),
      tail: tail.as_ref().map(|t| Self::build(env, t)),
    }.into()
  }

  /// From Erl binary op create a Core binaryop node (rewrap and rebuild both argument trees)
  fn core_from_binaryop(env: &Module, loc: &SourceLoc,
                        binop: &&ErlBinaryOperatorExpr) -> Arc<CoreAst> {
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

  /// From Erl Literal create a Core Variable (rewrap)
  fn core_from_literal(loc: &SourceLoc, lit: &Arc<Literal>) -> Arc<CoreAst> {
    CoreAst::Lit {
      location: loc.clone(),
      value: lit.clone(),
      // lit_type: lit.get_type()
    }.into()
  }

  /// From Erl Variable create a Core Variable (1:1 translation)
  fn core_from_var(erl_var: &ErlVar) -> Arc<CoreAst> {
    let core_var = Var {
      location: erl_var.location.clone(),
      name: Some(erl_var.name.clone()),
      ty: TypeVar::new(),
    };
    CoreAst::Var(core_var).into()
  }

  /// Given a left side of Application Expr(Args, ...) try to resolve atom function name in Expr
  /// to a real existing function in this module.
  fn core_from_apply_target(env: &Module,
                            expr_ast: &Arc<ErlAst>, apply: &ErlApply) -> Arc<CoreAst> {
    if let Ok(reg) = env.registry.read() {
      match reg.find_function_by_expr_arity(expr_ast, apply.args.len()) {
        Some(index) => {
          let fndef = reg.functions[index].clone();
          return CoreAst::new_fnref(fndef.funarity.clone()).into();
        }
        // TODO: external function references Mod:Fun/Arity
        // TODO: tuple calls? Literal::Tuple(_) => {}
        _ => {}
      }
    }
    Self::build(env, expr_ast)
  }

  /// From Erl Application create a Core AST application node
  /// Convert atom function name references to functionrefs by the known arity and module env
  fn core_from_apply(env: &Module, app: &ErlApply) -> Arc<CoreAst> {
    let target = match app.expr.deref() {
      ErlAst::Lit { .. } => Self::core_from_apply_target(env, &app.expr, app),
      _other => Self::build(env, &app.expr),
    };
    let core_app = Apply {
      location: app.location.clone(),
      target,
      args: app.args.iter()
          .map(|each_arg| Self::build(env, each_arg))
          .collect(),
      ret_ty: TypeVar::new(),
    };
    CoreAst::Apply(core_app).into()
  }
}
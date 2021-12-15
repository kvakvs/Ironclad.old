//! Contains code to build Core AST from Erlang AST
use function_name::named;
use std::sync::Arc;
use std::ops::Deref;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::core_erlang::syntax_tree::node::core_expr::BinaryOperatorExpr;
use crate::core_erlang::syntax_tree::node::core_var::Var;
use crate::core_erlang::syntax_tree::node::core_apply::Apply;
use crate::erl_error::ErlResult;
use crate::project::module::Module;
use crate::erlang::syntax_tree::node::erl_apply::ErlApply;
use crate::erlang::syntax_tree::node::erl_var::ErlVar;
use crate::source_loc::SourceLoc;
use crate::literal::Literal;
use crate::erlang::syntax_tree::node::erl_expression::ErlBinaryOperatorExpr;
use crate::typing::erl_type::ErlType;

mod fn_def_builder;

/// Dummy struct containing building code for Core AST from Erlang AST
pub struct CoreAstBuilder {}

impl CoreAstBuilder {
  /// Rebuild Core Erlang AST from Erlang AST
  #[named]
  pub fn build(env: &Module, ast: &Arc<ErlAst>) -> ErlResult<Arc<CoreAst>> {
    match ast.deref() {
      ErlAst::Empty => Ok(CoreAst::Empty.into()),
      ErlAst::ModuleAttr { name, .. } => {
        let module_as_core = CoreAst::Module {
          name: name.clone(),
          exports: vec![],
        };
        Ok(module_as_core.into())
      },
      ErlAst::ModuleForms(forms) => {
        let fn_defs: ErlResult<_> = forms.iter()
            .map(|each_form| Self::build(env, each_form))
            .collect();
        Ok(CoreAst::ModuleFuns(fn_defs?).into())
      }
      ErlAst::FnDef { .. } => Self::core_from_fndef(env, ast),
      // ErlAst::CClause(_, _) => {}
      // ErlAst::MFA { .. } => {}
      ErlAst::Var(erl_var) => Self::core_from_var(erl_var),
      ErlAst::Apply(app) => Self::core_from_apply(env, app),
      // ErlAst::Case(_, _) => {}
      ErlAst::Lit { location: loc, value: lit } => Self::core_from_literal(loc, lit),
      ErlAst::BinaryOp(loc, binop) => {
        Self::core_from_binaryop(env, loc, &binop)
      }
      // ErlAst::UnaryOp(_, _) => {}
      ErlAst::List { location, elements, tail } => {
        Self::core_from_list(env, location, elements, tail)
      }
      ErlAst::Tuple { location, elements, .. } => {
        Self::core_from_tuple(env, location, elements)
      }

      other => unimplemented!("{}: Don't know how to convert ErlAst {:?} into CoreAst",
                              function_name!(), other)
    }
  }

  /// From Erl List rewrap a Core List
  fn core_from_list(env: &Module, location: &SourceLoc,
                    elements: &[Arc<ErlAst>],
                    tail: &Option<Arc<ErlAst>>) -> ErlResult<Arc<CoreAst>> {
    let elements_as_core: ErlResult<_> = elements.iter()
        .map(|each_el| Self::build(env, each_el))
        .collect();
    let list_as_core = CoreAst::List {
      location: location.clone(),
      elements: elements_as_core?,
      tail: tail.as_ref().map(|t| Self::build(env, t).unwrap()),
    };
    Ok(list_as_core.into())
  }

  /// From Erl tuple rewrap a Core tuple
  fn core_from_tuple(env: &Module, location: &SourceLoc,
                     elements: &[Arc<ErlAst>]) -> ErlResult<Arc<CoreAst>> {
    let elements_as_core: ErlResult<_> = elements.iter()
        .map(|each_el| Self::build(env, each_el))
        .collect();
    let tuple_as_core = CoreAst::Tuple {
      location: location.clone(),
      elements: elements_as_core?,
    };
    Ok(tuple_as_core.into())
  }

  /// From Erl binary op create a Core binaryop node (rewrap and rebuild both argument trees)
  fn core_from_binaryop(env: &Module, loc: &SourceLoc,
                        binop: &&ErlBinaryOperatorExpr) -> ErlResult<Arc<CoreAst>> {
    let result = CoreAst::BinOp {
      location: loc.clone(),
      op: BinaryOperatorExpr {
        left: Self::build(env, &binop.left)?,
        right: Self::build(env, &binop.right)?,
        operator: binop.operator.into(),
      },
    };
    Ok(result.into())
  }

  /// From Erl Literal create a Core Variable (rewrap)
  fn core_from_literal(loc: &SourceLoc, lit: &Arc<Literal>) -> ErlResult<Arc<CoreAst>> {
    let lit_as_core = CoreAst::Lit {
      location: loc.clone(),
      value: lit.clone(),
      // lit_type: lit.get_type()
    };
    Ok(lit_as_core.into())
  }

  /// From Erl Variable create a Core Variable (1:1 translation)
  fn core_from_var(erl_var: &ErlVar) -> ErlResult<Arc<CoreAst>> {
    let core_var = Var {
      location: erl_var.location.clone(),
      name: erl_var.name.clone(),
      ty: ErlType::Any.into(),
    };
    Ok(CoreAst::Var(core_var.into()).into())
  }

  /// Given a left side of Application Expr(Args, ...) try to resolve atom function name in Expr
  /// to a real existing function in this module.
  fn core_from_apply_target(env: &Module,
                            expr_ast: &Arc<ErlAst>, apply: &ErlApply) -> ErlResult<Arc<CoreAst>> {
    if let Ok(reg) = env.registry.read() {
      if let Some(index) = reg.find_function_by_expr_arity(expr_ast, apply.args.len()) {
        let fndef = reg.functions[index].clone();
        return Ok(CoreAst::new_fnref(fndef.funarity.clone()).into());
      }
    }
    Self::build(env, expr_ast)
  }

  /// From Erl Application create a Core AST application node
  /// Convert atom function name references to functionrefs by the known arity and module env
  fn core_from_apply(env: &Module, app: &ErlApply) -> ErlResult<Arc<CoreAst>> {
    let target = match app.expr.deref() {
      ErlAst::Lit { .. } => Self::core_from_apply_target(env, &app.expr, app)?,
      _other => Self::build(env, &app.expr)?,
    };
    let args_as_core: ErlResult<_> = app.args.iter()
        .map(|each_arg| Self::build(env, each_arg))
        .collect();
    let core_app = Apply {
      location: app.location.clone(),
      target,
      args: args_as_core?,
    };
    Ok(CoreAst::Apply(core_app).into())
  }
}
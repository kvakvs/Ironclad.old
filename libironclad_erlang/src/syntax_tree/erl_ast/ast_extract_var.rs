//! AST node-type checks

use crate::syntax_tree::erl_ast::ast_iter::AstNode;
use crate::syntax_tree::erl_ast::{ErlAst, ErlAstType};
use crate::syntax_tree::erl_error::ErlError;
use crate::syntax_tree::node::erl_binary_element::ValueWidth;
use crate::typing::erl_type::ErlType;
use libironclad_error::ic_error::IcResult;
use std::collections::HashMap;
use std::sync::Arc;

impl ErlAst {
  /// For a function header, `myfun(A, {B, C}, #{key => D})` extract variable names: A, B, C, D.
  /// and add them to the scope of this function. Some AST nodes are not acceptable in argument
  /// list, so they would cause an error.
  pub fn extract_variables(
    node: &Arc<ErlAst>,
    variables: &mut HashMap<String, Arc<ErlType>>,
  ) -> IcResult<()> {
    match &node.content {
      ErlAstType::Var(v) => {
        variables.insert(v.name.clone(), ErlType::any());
        Ok(())
      }
      ErlAstType::BinaryExpr { elements, .. } => {
        for e in elements {
          Self::extract_variables(&e.value, variables)?;
          if let ValueWidth::Expr(expr_width) = &e.width {
            Self::extract_variables(expr_width, variables)?;
          }
        }
        Ok(())
      }
      ErlAstType::CommaExpr { elements, .. } => {
        for e in elements {
          Self::extract_variables(e, variables)?;
        }
        Ok(())
      }
      ErlAstType::ListComprehension { expr, generators, .. } => {
        Self::extract_variables(expr, variables)?;
        for g in generators {
          Self::extract_variables(g, variables)?;
        }
        Ok(())
      }
      ErlAstType::ListComprehensionGenerator { left, right, .. } => {
        Self::extract_variables(left, variables)?;
        Self::extract_variables(right, variables)
      }
      ErlAstType::IfStatement { clauses, .. } => {
        for clause in clauses {
          if let Some(children) = clause.children() {
            for child in children {
              Self::extract_variables(&child, variables)?;
            }
          }
        }
        Ok(())
      }
      ErlAstType::TryCatch { body, of_branches, catch_clauses, .. } => {
        Self::extract_variables(body, variables)?;
        if let Some(ofb) = of_branches {
          for b in ofb {
            Self::extract_variables(&b.body, variables)?;
            Self::extract_variables(&b.pattern, variables)?;
            if let Some(bguard) = &b.guard {
              Self::extract_variables(bguard, variables)?;
            }
          }
        }
        for cc in catch_clauses {
          Self::extract_variables(&cc.exc_pattern.class, variables)?;
          Self::extract_variables(&cc.exc_pattern.error, variables)?;
          Self::extract_variables(&cc.body, variables)?;
          if let Some(stk) = &cc.exc_pattern.stack {
            Self::extract_variables(stk, variables)?;
          }
          if let Some(wheng) = &cc.when_guard {
            Self::extract_variables(wheng, variables)?;
          }
        }
        Ok(())
      }
      ErlAstType::List { elements, tail, .. } => {
        for e in elements {
          Self::extract_variables(e, variables)?;
        }
        if let Some(tail_) = tail {
          Self::extract_variables(tail_, variables)?;
        }
        Ok(())
      }
      ErlAstType::Tuple { elements, .. } => {
        for e in elements {
          Self::extract_variables(e, variables)?;
        }
        Ok(())
      }
      ErlAstType::Map { values, .. } => {
        // Cannot bind variable to a map key in arguments list
        for v in values {
          Self::extract_variables(v, variables)?;
        }
        Ok(())
      }

      ErlAstType::Empty
      | ErlAstType::BinaryOp { .. }
      | ErlAstType::Lit { .. }
      | ErlAstType::Token { .. } => {
        Ok(()) // do nothing
      }

      ErlAstType::FnSpec { .. }
      | ErlAstType::Type { .. }
      | ErlAstType::MFA { .. }
      | ErlAstType::ModuleStartAttr { .. }
      | ErlAstType::ExportAttr { .. }
      | ErlAstType::ExportTypeAttr { .. }
      | ErlAstType::TypeAttr { .. }
      | ErlAstType::ImportAttr { .. }
      | ErlAstType::ModuleForms(_)
      | ErlAstType::FnRef { .. }
      | ErlAstType::FnDef(_)
      | ErlAstType::CClause(_, _)
      | ErlAstType::CaseStatement { .. }
      | ErlAstType::Apply(_)
      | ErlAstType::UnaryOp { .. }
      | ErlAstType::GenericAttr { .. } => ErlError::unacceptable(
        &node.location,
        format!("{}: is unacceptable as a function argument", node),
      ),
    }
  }
}

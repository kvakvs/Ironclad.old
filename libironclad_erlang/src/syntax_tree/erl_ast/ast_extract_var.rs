//! AST node-type checks

use crate::syntax_tree::erl_ast::ast_iter::AstNode;
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::erl_error::ErlError;
use crate::syntax_tree::node::erl_binary_element::ValueWidth;
use crate::typing::erl_type::ErlType;
use libironclad_error::ic_error::IcResult;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

impl ErlAst {
  /// For a function header, `myfun(A, {B, C}, #{key => D})` extract variable names: A, B, C, D.
  /// and add them to the scope of this function. Some AST nodes are not acceptable in argument
  /// list, so they would cause an error.
  pub fn extract_variables(node: &Arc<ErlAst>, variables: &mut HashMap<String, Arc<ErlType>>) -> IcResult<()> {
    match node.deref() {
      ErlAst::Var(v) => {
        variables.insert(v.name.clone(), ErlType::any());
        Ok(())
      }
      ErlAst::BinaryExpr { elements, .. } => {
        for e in elements {
          Self::extract_variables(&e.value, variables)?;
          if let ValueWidth::Expr(expr_width) = &e.width {
            Self::extract_variables(expr_width, variables)?;
          }
        }
        Ok(())
      }
      ErlAst::CommaExpr { elements, .. } => {
        for e in elements {
          Self::extract_variables(e, variables)?;
        }
        Ok(())
      }
      ErlAst::ListComprehension { expr, generators, .. } => {
        Self::extract_variables(expr, variables)?;
        for g in generators {
          Self::extract_variables(g, variables)?;
        }
        Ok(())
      }
      ErlAst::ListComprehensionGenerator { left, right, .. } => {
        Self::extract_variables(left, variables)?;
        Self::extract_variables(right, variables)
      }
      ErlAst::IfStatement { clauses, .. } => {
        for clause in clauses {
          if let Some(children) = clause.children() {
            for child in children {
              Self::extract_variables(&child, variables)?;
            }
          }
        }
        Ok(())
      }
      ErlAst::TryCatch { body, of_branches, catch_clauses, .. } => {
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
      ErlAst::List { elements, tail, .. } => {
        for e in elements {
          Self::extract_variables(e, variables)?;
        }
        if let Some(tail_) = tail {
          Self::extract_variables(tail_, variables)?;
        }
        Ok(())
      }
      ErlAst::Tuple { elements, .. } => {
        for e in elements {
          Self::extract_variables(e, variables)?;
        }
        Ok(())
      }
      ErlAst::Map { values, .. } => {
        // Cannot bind variable to a map key in arguments list
        for v in values {
          Self::extract_variables(v, variables)?;
        }
        Ok(())
      }

      ErlAst::Empty | ErlAst::BinaryOp { .. } | ErlAst::Lit { .. } | ErlAst::Token { .. } => {
        Ok(()) // do nothing
      }

      ErlAst::FnSpec { .. }
      | ErlAst::Type { .. }
      | ErlAst::MFA { .. }
      | ErlAst::ModuleStartAttr { .. }
      | ErlAst::ExportAttr { .. }
      | ErlAst::ExportTypeAttr { .. }
      | ErlAst::TypeAttr { .. }
      | ErlAst::ImportAttr { .. }
      | ErlAst::ModuleForms(_)
      | ErlAst::FnRef { .. }
      | ErlAst::FnDef(_)
      | ErlAst::CClause(_, _)
      | ErlAst::CaseStatement { .. }
      | ErlAst::Apply(_)
      | ErlAst::UnaryOp { .. }
      | ErlAst::GenericAttr { .. } => {
        ErlError::unacceptable(node.location(), format!("{}: is unacceptable as a function argument", node))
      }
    }
  }
}

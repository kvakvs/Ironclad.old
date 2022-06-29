//! AST node-type checks

use crate::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::node::erl_binary_element::ValueWidth;
use crate::error::ic_error::IcResult;
use crate::typing::erl_type::{ErlType, ErlTypeImpl};
use std::collections::HashMap;

impl AstNodeImpl {
  /// For a function header, `myfun(A, {B, C}, #{key => D})` extract variable names: A, B, C, D.
  /// and add them to the scope of this function. Some AST nodes are not acceptable in argument
  /// list, so they would cause an error.
  pub(crate) fn extract_variables(
    node: &AstNode,
    variables: &mut HashMap<String, ErlType>,
  ) -> IcResult<()> {
    match &node.content {
      AstNodeType::Var(v) => {
        variables.insert(v.name.clone(), ErlTypeImpl::any());
        Ok(())
      }
      AstNodeType::BinaryExpr { elements, .. } => {
        for e in elements {
          Self::extract_variables(&e.value, variables)?;
          if let ValueWidth::Expr(expr_width) = &e.width {
            Self::extract_variables(expr_width, variables)?;
          }
        }
        Ok(())
      }
      AstNodeType::CommaExpr { elements, .. } => {
        for e in elements {
          Self::extract_variables(e, variables)?;
        }
        Ok(())
      }
      AstNodeType::ListComprehension { expr, generators, .. } => {
        Self::extract_variables(expr, variables)?;
        for g in generators {
          Self::extract_variables(g, variables)?;
        }
        Ok(())
      }
      AstNodeType::BinaryComprehension { expr, generators, .. } => {
        Self::extract_variables(expr, variables)?;
        for g in generators {
          Self::extract_variables(g, variables)?;
        }
        Ok(())
      }
      AstNodeType::ListComprehensionGenerator { left, right, .. } => {
        Self::extract_variables(left, variables)?;
        Self::extract_variables(right, variables)
      }
      AstNodeType::IfStatement { clauses, .. } => {
        for clause in clauses {
          if let Some(children) = clause.children() {
            for child in children {
              Self::extract_variables(&child, variables)?;
            }
          }
        }
        Ok(())
      }
      AstNodeType::BeginEnd { exprs } => {
        for expr in exprs {
          Self::extract_variables(&expr, variables)?;
        }
        Ok(())
      }
      AstNodeType::TryCatch { body, of_branches, catch_clauses, .. } => {
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
      AstNodeType::List { elements, tail, .. } => {
        for e in elements {
          Self::extract_variables(e, variables)?;
        }
        if let Some(tail_) = tail {
          Self::extract_variables(tail_, variables)?;
        }
        Ok(())
      }
      AstNodeType::Tuple { elements, .. } => {
        for e in elements {
          Self::extract_variables(e, variables)?;
        }
        Ok(())
      }
      AstNodeType::MapBuilder { members, .. } => {
        // Cannot bind variable to a map key in arguments list
        for v in members {
          Self::extract_variables(&v.key, variables)?;
          Self::extract_variables(&v.expr, variables)?;
        }
        Ok(())
      }
      AstNodeType::RecordBuilder { base, tag, members, .. } => {
        variables.insert(tag.clone(), ErlTypeImpl::new_record_ref(tag.clone()));
        for v in members {
          Self::extract_variables(&v.expr, variables)?;
        }
        Ok(())
      }

      AstNodeType::Empty { .. } | AstNodeType::BinaryOp { .. } | AstNodeType::Lit { .. } => {
        Ok(()) // do nothing
      }

      AstNodeType::Type { .. }
      | AstNodeType::MFA { .. }
      | AstNodeType::ModuleRoot { .. }
      | AstNodeType::FnRef { .. }
      | AstNodeType::FnDef(_)
      | AstNodeType::CClause(_, _)
      | AstNodeType::CaseStatement { .. }
      | AstNodeType::Apply(_)
      | AstNodeType::UnaryOp { .. } => ErlError::unacceptable(
        node.location.clone(),
        format!("{}: is unacceptable as a function argument", node),
      ),
    }
  }
}

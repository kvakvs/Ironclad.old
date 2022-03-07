//! AST node-type checks

use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;
use crate::erl_error::{ErlError, ErlResult};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;

impl ErlAst {
  /// For a function header, `myfun(A, {B, C}, #{key => D})` extract variable names: A, B, C, D.
  /// and add them to the scope of this function. Some AST nodes are not acceptable in argument
  /// list, so they would cause an error.
  pub fn extract_variables(node: &Arc<ErlAst>, variables: &mut HashMap<String, Arc<ErlType>>) -> ErlResult<()> {
    match node.deref() {
      ErlAst::Var(v) => {
        variables.insert(v.name.clone(), ErlType::any());
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

      ErlAst::Empty
      | ErlAst::Lit { .. }
      | ErlAst::Token { .. } => {
        Ok(())  // do nothing
      }

      ErlAst::FnSpec { .. }
      | ErlAst::Type { .. }
      | ErlAst::MFA { .. }
      | ErlAst::ModuleStartAttr { .. }
      | ErlAst::ExportAttr { .. }
      | ErlAst::ModuleForms(_)
      | ErlAst::FnRef { .. }
      | ErlAst::FnDef(_)
      | ErlAst::CClause(_, _)
      | ErlAst::Case(_, _)
      | ErlAst::Apply(_)
      | ErlAst::BinaryOp { .. }
      | ErlAst::UnaryOp { .. }
      | ErlAst::UnparsedAttr { .. } => {
        ErlError::unacceptable_ast(format!("{}", node), "function argument".to_string())
      }
    }
  }
}
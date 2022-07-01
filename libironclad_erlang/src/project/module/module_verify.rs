//! Checks after parsing, to see that fn specs, exports, etc are not orphaned

use crate::erl_syntax::erl_ast::expr_style::ExprStyle;
use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::error::ic_error::IcResult;
use crate::project::module::module_impl::{ErlModule, ErlModuleImpl};
use nom::combinator::verify;

impl ErlModuleImpl {
  /// Check that specs and exports match the fn defs
  /// TODO: Check that func specs match func defs
  ///  TODO: Check that exports all exist as funs
  pub fn verify_preprocessed_integrity(&self) -> IcResult<()> {
    Ok(())
  }

  fn verify_parsed(&self, ast: &AstNode) -> IcResult<()> {
    match &ast.content {
      AstNodeType::ModuleForms { forms } => {
        for f in forms.iter() {
          self.verify_parsed(f)?;
        }
        Ok(())
      }
      AstNodeType::FnDef(fndef) => {
        for c in fndef.clauses.iter() {
          for arg in c.args.iter() {
            AstNodeImpl::verify_expr_style(arg, ExprStyle::MatchExpr)?;
          }
          self.verify_parsed(&c.body)?;
        }
        Ok(())
      }

      // AstNodeType::CClause(_, _) => {}
      // AstNodeType::Apply(_) => {}
      // AstNodeType::CaseExpr { .. } => {}
      // AstNodeType::BinaryOp { .. } => {}
      // AstNodeType::UnaryOp { .. } => {}
      // AstNodeType::List { .. } => {}
      // AstNodeType::Tuple { .. } => {}
      // AstNodeType::MapBuilder { .. } => {}
      // AstNodeType::RecordBuilder { .. } => {}
      // AstNodeType::RecordField { .. } => {}
      // AstNodeType::CommaExpr { .. } => {}
      // AstNodeType::ListComprehension { .. } => {}
      // AstNodeType::BinaryComprehension { .. } => {}
      // AstNodeType::ListComprehensionGenerator { .. } => {}
      // AstNodeType::TryCatch { .. } => {}
      // AstNodeType::IfStatement { .. } => {}
      // AstNodeType::BeginEnd { .. } => {}
      // AstNodeType::BinaryExpr { .. } => {}
      //-----------------------------------
      // No check necessary for these nodes
      //-----------------------------------
      AstNodeType::FnRef { .. }
      | AstNodeType::Type { .. }
      | AstNodeType::MFA { .. }
      | AstNodeType::Lit { .. }
      | AstNodeType::Var(_) => Ok(()),
      //-----------------------------------
      _ => unimplemented!("ErlModule::verify_parsed: don't know how to handle {:?}", ast.content),
    }
  }

  /// Check that expression nodes do not contain forbidden node types
  pub fn verify_parsed_integrity(&self) -> IcResult<()> {
    self.verify_parsed(&*self.ast.borrow())
  }
}

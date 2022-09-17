//! Checks after parsing, to see that fn specs, exports, etc are not orphaned

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_callable_target::CallableTarget;
use crate::error::ic_error::IroncladResult;
use crate::project::module::module_impl::ErlModuleImpl;

impl ErlModuleImpl {
  /// Check that specs and exports match the fn defs
  /// TODO: Check that func specs match func defs
  ///  TODO: Check that exports all exist as funs
  pub fn verify_preprocessed_integrity(&self) -> IroncladResult<()> {
    Ok(())
  }

  #[allow(clippy::only_used_in_recursion)]
  fn verify_parsed(&self, ast: &AstNode) -> IroncladResult<()> {
    match &ast.content {
      AstNodeType::ModuleForms { forms } => {
        for f in forms.iter() {
          self.verify_parsed(f)?;
        }
      }
      AstNodeType::FnDef(fndef) => {
        for c in fndef.clauses.iter() {
          for arg in c.args.iter() {
            AstNodeImpl::verify_expr_is_matchexpr(arg)?;
          }
          self.verify_parsed(&c.body)?;
        }
      }
      AstNodeType::CommaExpr { elements } => {
        for e in elements.iter() {
          self.verify_parsed(e)?;
        }
      }
      AstNodeType::Apply(app) => {
        match &app.target {
          CallableTarget::Expr(ex) => {
            self.verify_parsed(ex)?;
          }
          CallableTarget::MFArity(_) => {}
          CallableTarget::MFAExpression { module, function, .. } => {
            if let Some(m) = module {
              self.verify_parsed(m)?;
            }
            self.verify_parsed(function)?;
          }
        }
        for a in app.args.iter() {
          self.verify_parsed(a)?;
        }
      }
      AstNodeType::BinaryOp { binop_expr } => {
        self.verify_parsed(&binop_expr.left)?;
        self.verify_parsed(&binop_expr.right)?;
      }
      AstNodeType::UnaryOp { unop_expr } => {
        self.verify_parsed(&unop_expr.expr)?;
      }
      AstNodeType::List { elements, tail } => {
        for e in elements.iter() {
          self.verify_parsed(e)?;
        }
        if let Some(t) = tail {
          self.verify_parsed(t)?;
        }
      }
      AstNodeType::Tuple { elements } => {
        for e in elements.iter() {
          self.verify_parsed(e)?;
        }
      }
      AstNodeType::MapBuilder { base, members, .. } => {
        if let Some(b) = base {
          self.verify_parsed(b)?;
        }
        for m in members.iter() {
          self.verify_parsed(&m.key)?;
          self.verify_parsed(&m.expr)?;
        }
      }
      AstNodeType::RecordBuilder { base, members, .. } => {
        if let Some(b) = base {
          self.verify_parsed(b)?;
        }
        for m in members.iter() {
          self.verify_parsed(&m.expr)?;
        }
      }
      AstNodeType::ListComprehension { expr, generators } => {
        self.verify_parsed(expr)?;
        for g in generators.iter() {
          self.verify_parsed(g)?;
        }
      }
      AstNodeType::BinaryComprehension { expr, generators } => {
        self.verify_parsed(expr)?;
        for g in generators.iter() {
          self.verify_parsed(g)?;
        }
      }
      AstNodeType::TryCatch { body, of_branches, catch_clauses } => {
        self.verify_parsed(body)?;
        if let Some(of_branches_vec) = of_branches {
          for ofb in of_branches_vec {
            AstNodeImpl::verify_expr_is_matchexpr(&ofb.pattern)?;
            self.verify_parsed(&ofb.body)?;

            if let Some(guard) = &ofb.guard {
              AstNodeImpl::verify_expr_is_matchexpr(guard)?;
            }
          }
        }
        for cc in catch_clauses.iter() {
          AstNodeImpl::verify_expr_is_matchexpr(&cc.exc_pattern.class)?;
          AstNodeImpl::verify_expr_is_matchexpr(&cc.exc_pattern.error)?;
          if let Some(guard) = &cc.when_guard {
            AstNodeImpl::verify_expr_is_matchexpr(guard)?;
          }
          self.verify_parsed(&cc.body)?;
        }
      }
      AstNodeType::ListComprehensionGenerator { left, right } => {
        self.verify_parsed(left)?;
        self.verify_parsed(right)?;
      }
      AstNodeType::IfStatement { clauses } => {
        for c in clauses.iter() {
          self.verify_parsed(&c.cond)?;
          self.verify_parsed(&c.body)?;
        }
        // TODO: Coverage analysis, must end with true or cover all cases?
      }
      AstNodeType::BeginEnd { exprs } => {
        for e in exprs.iter() {
          self.verify_parsed(e)?;
        }
      }
      AstNodeType::BinaryExpr { elements } => {
        for e in elements.iter() {
          self.verify_parsed(&e.value)?;
        }
      }
      AstNodeType::RecordField { base, .. } => {
        if let Some(b) = base {
          self.verify_parsed(b)?;
        }
      }
      //-----------------------------------
      AstNodeType::CaseExpr { expr, clauses } => {
        self.verify_parsed(expr)?;
        for c in clauses.iter() {
          self.verify_parsed(&c.pattern)?;
          if let Some(guard) = &c.guard {
            AstNodeImpl::verify_expr_is_guard(guard)?;
          }
          self.verify_parsed(&c.body)?;
        }
      }
      AstNodeType::CClause(_, _) => {
        unreachable!("Node must not occur in the wild: {:?}", &ast.content);
      }
      //-----------------------------------
      // No check necessary for these nodes
      //-----------------------------------
      AstNodeType::FnRef { .. }
      | AstNodeType::Type { .. }
      | AstNodeType::MFA { .. }
      | AstNodeType::Lit { .. }
      | AstNodeType::Var(_) => {}
      //-----------------------------------
      _ => unimplemented!("ErlModule::verify_parsed: don't know how to handle {:?}", ast.content),
    }
    Ok(())
  }

  /// Check that expression nodes do not contain forbidden node types
  pub fn verify_parsed_integrity(&self) -> IroncladResult<()> {
    self.verify_parsed(&self.ast.borrow())
  }
}

//! Provides a TypeEquation struct for adding type equality/match constraints to the program

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;
use crate::erl_error::{ErlResult, ErlError};
use std::rc::Rc;
use std::borrow::Borrow;

/// Type equation, assumes matching or equal types, t1 = t2
pub struct TypeEquation {
  /// Left type of equation of t1 = t2, must equal (match) the right type
  pub left: ErlType,
  /// Right type of equation of t1 = t2
  pub right: ErlType,
  /// The reference to the AST node which generated this equation
  pub node: Rc<ErlAst>,
}

impl TypeEquation {
  /// Create a new type equation
  pub fn new(node: &Rc<ErlAst>, ty1: ErlType, ty2: ErlType) -> Self {
    Self {
      left: ty1,
      right: ty2,
      node: node.clone(),
    }
  }

  /// Generate type equations from node. Each type variable is opposed to some type which we know, or
  /// to Any, if we don't know.
  pub fn generate_equations(ast: &Rc<ErlAst>, result: &mut Vec<TypeEquation>) -> ErlResult<()> {
    match ast.get_children() {
      Some(c) => {
        let (_, errors): (Vec<_>, Vec<_>) =
            c.iter()
                .map(|ast_node| { // drop OK results, keep errors
                  Self::generate_equations(ast_node, result)
                })
                .partition(Result::is_ok);
        let mut errors: Vec<ErlError> = errors.into_iter().map(Result::unwrap_err).collect();
        match errors.len() {
          0 => (),
          1 => return Err(errors.pop().unwrap()),
          _ => return Err(ErlError::Multiple(errors)),
        }
      }
      None => {}
    }

    match ast.borrow() {
      ErlAst::Forms(_) => unreachable!("Must not call generate_equations() on Forms"),
      ErlAst::ModuleAttr { .. } => {}
      ErlAst::Lit(_) => {} // creates no equation, type is known
      ErlAst::NewFunction { ret, clauses, .. } => {
        // Return type of a function is union of its clauses return types
        let union_t = ErlType::new_union(clauses.iter().map(|c| c.get_type()).collect());
        result.push(TypeEquation::new(ast, ret.clone(), union_t));
      }
      ErlAst::FClause { ret, body, .. } => {
        // For each fun clause its return type is matched with body expression type
        result.push(TypeEquation::new(ast, ret.clone(), body.get_type()));
      }
      ErlAst::Var { .. } => {}
      ErlAst::App { expr, args, ty } => {
        let fn_type = ErlType::new_fun(
          None, // unnamed function application
          args.iter()
              .map(|a| a.get_type())
              .collect(),
          ty.clone());
        // A callable expression node type must match the supplied arguments types
        result.push(TypeEquation::new(ast, expr.get_type(), fn_type));
      }
      ErlAst::Let { in_ty, in_expr, .. } => {
        result.push(TypeEquation::new(ast, in_ty.clone(), in_expr.get_type()));
      }
      ErlAst::Case { ty, clauses, .. } => {
        // For Case expression, type of case must be union of all clause types
        let all_clause_types = clauses.iter()
            .map(|c| c.get_type())
            .collect();
        let union_t = ErlType::new_union(all_clause_types);
        result.push(TypeEquation::new(ast, ty.clone(), union_t));
      }
      ErlAst::CClause { guard, body, ty, .. } => {
        // Clause type must match body type
        result.push(TypeEquation::new(ast, ty.clone(), body.get_type()));
        // No check for clause condition, but the clause condition guard must be boolean
        result.push(TypeEquation::new(ast, guard.get_type(), ErlType::Bool));
      }
      ErlAst::BinaryOp { left, right, op, ty } => {
        // Check result of the binary operation
        result.push(TypeEquation::new(ast, ty.clone(), op.get_result_type()));

        match op.get_arg_type() {
          Some(arg_type) => {
            // Both sides of a binary op must have type appropriate for that op
            result.push(TypeEquation::new(ast, left.get_type(), arg_type.clone()));
            result.push(TypeEquation::new(ast, right.get_type(), arg_type));
          }
          None => {}
        }
      }
      ErlAst::UnaryOp { expr, op } => {
        // Equation of expression type must match either bool for logical negation,
        // or (int|float) for numerical negation
        result.push(TypeEquation::new(ast,
                                      expr.get_type(),
                                      op.get_type()));
        // TODO: Match return type with inferred return typevar?
      }
      ErlAst::Comma { right, ty, .. } => {
        result.push(TypeEquation::new(ast,
                                      ty.clone(),
                                      right.get_type()));
      }
      _ => unreachable!("Can't process {:?}", ast),
    }
    Ok(())
  }
}

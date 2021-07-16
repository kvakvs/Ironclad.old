pub mod erl_type;
pub mod error;
pub mod typevar;
pub mod equation;
pub mod unifier;

#[cfg(test)]
mod tests {
  use crate::syntaxtree::erl::erl_ast::ErlAst;
  use crate::typing::equation::{TypeEquation};
  use crate::syntaxtree::erl::literal::ErlLiteral;
  use crate::syntaxtree::erl::erl_op::ErlBinaryOp;
  use crate::typing::erl_type::ErlType;
  use std::rc::Rc;
  use crate::typing::unifier::Unifier;

  /// Build AST for a function
  /// myfun(A) -> (A + 1) / 2.
  /// The inferred type should be: float() (as return type)
  #[test]
  fn simple_inference_test() {
    let clause1_expr1 = ErlAst::new_binop(
      ErlAst::new_var("A"),
      ErlBinaryOp::Add,
      Rc::new(ErlAst::Lit(ErlLiteral::Integer(1))));
    let clause1_body = ErlAst::new_binop(
      clause1_expr1,
      ErlBinaryOp::Div,
      Rc::new(ErlAst::Lit(ErlLiteral::Integer(2))));
    let clause1 = ErlAst::new_fclause(
      vec![ErlAst::new_var("A")],
      clause1_body.clone());
    let erl_fn = ErlAst::new_fun("test1", vec![clause1]);
    println!("Fun: {:?}", erl_fn);

    let mut equations: Vec<TypeEquation> = Vec::new();
    TypeEquation::generate_equations(&erl_fn, &mut equations)
        .unwrap();

    println!("Equations: {:?}", equations);

    let mut unifier = Unifier::unify_all_equations(equations).unwrap();
    println!("Unify map: {:?}", unifier.subst);

    println!("Inferred: {}", unifier.get_expression_type(erl_fn).to_string());
  }
}

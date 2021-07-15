pub mod erl_type;
pub mod error;
pub mod typevar;
pub mod equation;

#[cfg(test)]
mod tests {
  use crate::syntaxtree::erl::erl_ast::ErlAst;
  use crate::typing::equation::{TypeEquation};
  use crate::syntaxtree::erl::literal::ErlLiteral;
  use crate::syntaxtree::erl::erl_op::ErlBinaryOp;
  use crate::typing::erl_type::ErlType;

  #[test]
  fn expr1_test() {
    // Build AST for a function
    // fun(A) -> A + 1.
    // The inferred type should be: ((integer()) -> integer())
    let clause1_body = ErlAst::BinaryOp {
      left: Box::new(ErlAst::new_var("A")),
      right: Box::new(ErlAst::Lit(ErlLiteral::Integer(1))),
      op: ErlBinaryOp::Add,
      ty: ErlType::new_typevar(),
    };
    let clause1 = ErlAst::new_fclause(vec![ErlAst::new_var("A")], clause1_body);
    let erl_fn = ErlAst::new_fun("test1", vec![clause1]);
    println!("{:?}", erl_fn);

    let mut equations: Vec<TypeEquation> = Vec::new();
    TypeEquation::generate_equations(&erl_fn, &mut equations);
  }
}

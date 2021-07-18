extern crate compiler;
//mod test_util;

use compiler::syntaxtree::erl::erl_ast::{ErlAst, ErlAstTree};
use compiler::typing::equation::{TypeEquation};
use compiler::syntaxtree::erl::literal::ErlLit;
use compiler::syntaxtree::erl::erl_op::ErlBinaryOp;
use compiler::typing::unifier::Unifier;
use compiler::erl_error::ErlResult;
use std::rc::Rc;

/// Build AST for a function
/// myfun(A) -> (A + 1) / 2.
/// The inferred type should be: float() (as return type)
#[test]
fn simple_inference_test() {
  let clause1_expr1 = ErlAst::new_binop(
    ErlAst::new_var("A"),
    ErlBinaryOp::Add,
    Rc::new(ErlAst::Lit(ErlLit::Integer(1))));
  let clause1_body = ErlAst::new_binop(
    clause1_expr1.clone(),
    ErlBinaryOp::Div,
    Rc::new(ErlAst::Lit(ErlLit::Integer(2))));
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

  println!("Inferred for (A+1): {}",
           unifier.infer_ast(clause1_expr1).to_string());

  println!("Inferred for f(A) -> (A+1)/2.: {}",
           unifier.infer_ast(erl_fn.clone()).to_string());

  println!("Inferred for f(A): {}",
           unifier.infer_type(erl_fn.get_fun_type().unwrap()).to_string());
}

fn test_module(filename: &str, input: &str) -> ErlResult<ErlAstTree> {
  ErlAstTree::from_str(filename,
                       &(String::from("-module(test).\n") + input))
}

#[test]
fn simple_inference_test_2() {
  match test_module("<simple_inference_test_2>",
                    "myfun(A) -> (A + 1) / 2.") {
    Ok(tree) => println!("Parsed: {:?}", tree),
    Err(e) => println!("Failed {}", e),
  }
}

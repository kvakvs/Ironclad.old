extern crate compiler;

mod test_util;

use compiler::syntaxtree::erl::erl_ast::{ErlAst};
use compiler::typing::equation::{TypeEquation};
use compiler::syntaxtree::erl::literal::ErlLit;
use compiler::syntaxtree::erl::erl_op::ErlBinaryOp;
use compiler::typing::unifier::Unifier;
use std::rc::Rc;
use compiler::erl_error::ErlResult;
use compiler::syntaxtree::erl::erl_parser;
use std::ops::Deref;

/// Build AST for a function
/// myfun(A) -> (A + 1) / 2.
/// The inferred type should be: float() (as return type)
#[test]
#[ignore]
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
    "myfun",
    vec![ErlAst::new_var("A")],
    clause1_body.clone());
  let erl_fn = ErlAst::new_fun(vec![clause1]);
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

#[test]
fn parse_infer_test_1() -> ErlResult<()> {
  let tree = test_util::erl_parse(
    erl_parser::Rule::function_def,
    "myfun(A) -> (A + 1) / 2.")
      .unwrap();
  match tree.deref() {
    ErlAst::NewFunction { clauses, .. } => {
      assert_eq!(clauses.len(), 1, "NewFunction must have exact one clause");
      assert_eq!(clauses[0].get_fclause_name().unwrap(), "myfun", "FClause name must be myfun");

      match clauses[0].deref() {
        ErlAst::FClause { args, .. } => {
          assert_eq!(args.len(), 1, "FClause must have exact one arg");
          assert!(matches!(args[0].deref(), ErlAst::Var{..}), "FClause arg must be a Var node");
        }
        other2 => test_util::fail_unexpected(other2),
      }
    }
    other1 => test_util::fail_unexpected(other1),
  }
  println!("Parsed: {:?}", tree);
  Ok(())
}

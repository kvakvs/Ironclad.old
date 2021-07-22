extern crate compiler;

mod test_util;

use compiler::syntaxtree::erl::erl_ast::{ErlAst};
use compiler::typing::equation::{TypeEquation};
use compiler::typing::unifier::Unifier;
use compiler::erl_error::ErlResult;
use compiler::syntaxtree::erl::erl_parser::{Rule};
use std::ops::Deref;

#[test]
fn parse_infer_test_1() -> ErlResult<()> {
  let code = "myfun(A) -> (A + 1) / 2.";
  let erl_fn = test_util::erl_parse(Rule::function_def, code).unwrap();

  match erl_fn.deref() {
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
  println!("Parsed: {:?}", erl_fn);

  let mut equations: Vec<TypeEquation> = Vec::new();
  TypeEquation::generate_equations(&erl_fn, &mut equations)
      .unwrap();

  println!("Equations: {:?}", equations);

  let mut unifier = Unifier::unify_all_equations(equations).unwrap();
  println!("Unify map: {:?}", unifier.subst);

  let erl_fn_t = unifier.infer_ast(erl_fn.clone());
  println!("Inferred for {:?}: {:?}", erl_fn, erl_fn_t.into_final_type());

  // println!("Inferred for f(A): {}",
  //          unifier.infer_type(erl_fn.get_fun_type().unwrap()).to_string());

  Ok(())
}

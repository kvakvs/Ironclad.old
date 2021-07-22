extern crate compiler;

mod test_util;

use compiler::syntaxtree::erl::erl_ast::{ErlAst};
use compiler::typing::unifier::Unifier;
use compiler::erl_error::ErlResult;
use compiler::syntaxtree::erl::erl_parser::{Rule};
use std::ops::Deref;

#[test]
fn infer_simplemath() -> ErlResult<()> {
  let code = "myfun(A) -> (A + 1) / 2.";
  let ast = test_util::erl_parse(Rule::function_def, code).unwrap();

  match ast.deref() {
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
  println!("Parsed: {:?}", ast);

  let mut unifier = Unifier::new(ast.clone()).unwrap();
  let f_t = unifier.infer_ast(ast.clone());
  println!("Inferred for {:?} 🡆 {:?}", ast, f_t.into_final_type());

  // println!("Inferred for f(A): {}",
  //          unifier.infer_type(erl_fn.get_fun_type().unwrap()).to_string());

  Ok(())
}

#[test]
fn infer_funcall() -> ErlResult<()> {
  let code = "-module(infer_funcall).\n\
                   add(A, B) -> A + B.\n\
                   main() -> add(A, 4).\n";
  let ast = test_util::erl_parse(Rule::module, code).unwrap();
  let f_ast = ast.find_fun("main").unwrap();

  let mut unifier = Unifier::new(ast.clone()).unwrap();
  let f_t = unifier.infer_ast(f_ast.clone());
  println!("Inferred for {:?} 🡆 {:?}", f_ast, f_t.into_final_type());

  Ok(())
}
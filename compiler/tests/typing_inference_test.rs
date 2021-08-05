extern crate compiler;

mod test_util;

use compiler::syntaxtree::erl::erl_ast::{ErlAst};
use compiler::erl_error::ErlResult;
use compiler::syntaxtree::erl::erl_parser::{Rule};
use std::ops::Deref;
use compiler::typing::erl_type::ErlType;
use compiler::erl_module::ErlModule;

#[test]
fn infer_simplemath() -> ErlResult<()> {
  let code = "myfun(A) -> (A + 1) / 2.";
  let mut module = ErlModule::new_testing();
  module.parse_and_unify_str(Rule::function_def, code)?;

  {
    let ast = module.ast.read().unwrap();
    match ast.deref() {
      ErlAst::NewFunction(_loc, nf) => {
        assert_eq!(nf.clauses.len(), 1, "NewFunction must have exact one clause");
        assert_eq!(nf.funarity.arity, 1, "NewFunction must have arity 1");
        assert_eq!(nf.clauses[0].name, "myfun", "FClause name must be myfun");

        let fc = &nf.clauses[0];
        assert_eq!(fc.args.len(), 1, "FClause must have exact one arg");
        assert!(matches!(fc.args[0], ErlAst::Var{..}), "FClause arg must be a Var node");
      }
      other1 => test_util::fail_unexpected(other1),
    }
    println!("Parsed: {}", module.ast.read().unwrap());

    let f_t = module.unifier.infer_ast(&ast).into_final_type();
    println!("Inferred for {} 🡆 {}", &ast, f_t);
  }

  Ok(())
}

#[test]
fn infer_funcall_test() -> ErlResult<()> {
  println!("--- testing function call type inference ---");

  let code = "-module(infer_funcall).\n\
                   add(A, B) -> A + B.\n\
                   main() -> add(A, 4).\n";
  let mut module = ErlModule::new_testing();
  module.parse_and_unify_str(Rule::module, code)?;

  {
    let ast = module.ast.read().unwrap();
    let find_result1 = ast.find_fun("add", 2).unwrap();
    let f_t1 = module.unifier.infer_ast(find_result1.ast).into_final_type();
    println!("Inferred for {} 🡆 {}", find_result1.ast, f_t1);

    // Expected: in Add/2 -> number(), args A :: number(), B :: integer()
    assert_eq!(f_t1, ErlType::Number, "Function add/2 must have inferred type: number()");
  }

  {
    let ast2 = module.ast.read().unwrap();
    let find_result2 = ast2.find_fun("main", 0).unwrap();
    let f_t2 = module.unifier.infer_ast(find_result2.ast).into_final_type();
    println!("Inferred for {} 🡆 {}", find_result2.ast, f_t2);

    // Expected: Main -> integer()
    assert_eq!(f_t2, ErlType::Number, "Function main/0 must have inferred type: number()");
  }

  Ok(())
}
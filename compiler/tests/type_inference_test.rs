extern crate compiler;
extern crate function_name;

mod test_util;

use ::function_name::named;
use compiler::erlang::syntax_tree::erl_ast::{ErlAst};
use compiler::erl_error::ErlResult;
use compiler::erlang::syntax_tree::erl_parser::{Rule};
use std::ops::Deref;
use compiler::typing::erl_type::ErlType;
use compiler::mfarity::MFArity;
use compiler::project::module::Module;

#[named]
#[test]
fn infer_simplemath() -> ErlResult<()> {
  test_util::start(function_name!(), "infer type for simple expression");
  let code = "myfun(A) -> (A + 1) / 2.";
  let mut module = Module::default();
  module.parse_and_unify_str(Rule::function_def, code)?;

  {
    let ast = module.ast.read().unwrap().clone();
    match ast.deref() {
      ErlAst::FunctionDef { fn_def, .. } => {
        assert_eq!(fn_def.clauses.len(), 1, "FunctionDef must have exact one clause");
        assert_eq!(fn_def.mfarity.arity, 1, "FunctionDef must have arity 1");

        let clause = &fn_def.clauses[0];
        assert_eq!(clause.name, "myfun", "FClause name must be myfun");

        assert_eq!(clause.args.len(), 1, "FClause must have exact one arg");
        assert!(matches!(clause.args[0], ErlAst::Var{..}), "FClause arg must be a Var node");
      }
      other1 => test_util::fail_unexpected(other1),
    }
    println!("Parsed: {}", module.ast.read().unwrap());

    let f_t = module.unifier.infer_ast(&ast).into_final_type();
    println!("{}: Inferred {} 🡆 {}", function_name!(), &ast, f_t);
  }

  Ok(())
}

#[named]
#[test]
/// Try infer type for a function which is a sum of two lists.
/// Expected: Inferred type list(atom1|atom2)
fn infer_atom_list_concatenation() -> ErlResult<()> {
  test_util::start(function_name!(), "infer type for a sum of two lists with atoms");
  let code = "atomtest(A) -> [atom1] ++ [atom2].";
  let mut module = Module::default();
  module.parse_and_unify_str(Rule::function_def, code)?;
  {
    let ast = module.ast.read().unwrap();
    let f_t = module.unifier.infer_ast(&ast).into_final_type();
    println!("{}: Inferred {} 🡆 {}", function_name!(), &ast, f_t);
    if let ErlType::List(t) = &f_t {
      if let ErlType::Union(elems) = t.deref() {
        assert!(elems.contains(&ErlType::Atom(String::from("atom1"))));
        assert!(elems.contains(&ErlType::Atom(String::from("atom2"))));
      } else {
        panic!("{}: Function atomtest() must have inferred type [atom1|atom2], got [{}]",
               function_name!(), t)
      }
    } else {
      panic!("{}: Function atomtest() must have inferred type [atom1|atom2], got {}",
             function_name!(), f_t)
    };
  }
  Ok(())
}

#[named]
#[test]
fn infer_funcall_test() -> ErlResult<()> {
  test_util::start(function_name!(), "infer type for a fun which calls another fun with a sum");
  let code = "-module(infer_funcall).\n\
                   add(A, B) -> A + B.\n\
                   main() -> add(A, 4).\n";
  let mut module = Module::default();
  module.parse_and_unify_str(Rule::module, code)?;
  {
    let ast = module.ast.read().unwrap();
    let add_fn_ast = ast.find_function_def(&MFArity::new_local_str("add", 2)).unwrap();
    let f_t1 = module.unifier.infer_ast(add_fn_ast).into_final_type();
    println!("{}: Inferred {} 🡆 {}", function_name!(), add_fn_ast, f_t1);

    // Expected: in Add/2 -> number(), args A :: number(), B :: integer()
    assert_eq!(f_t1, ErlType::Number, "Function add/2 must have inferred type: number()");
  }

  {
    let ast2 = module.ast.read().unwrap();
    let find_result2 = ast2.find_function_def(&MFArity::new_local_str("main", 0)).unwrap();
    let f_t2 = module.unifier.infer_ast(find_result2).into_final_type();
    println!("{}: Inferred {} 🡆 {}", function_name!(), find_result2, f_t2);

    // Expected: Main -> integer()
    assert_eq!(f_t2, ErlType::Number, "Function main/0 must have inferred type: number()");
  }

  Ok(())
}

#[named]
#[test]
fn infer_multiple_clause_test() -> ErlResult<()> {
  test_util::start(function_name!(), "infer type for a multi-clause function");
  let code = "-module(infer_multiple_clause).\n\
                   main() -> [atom1] ++ [atom2];\n\
                   main() -> 2 + 3.\n";
  let mut module = Module::default();
  module.parse_and_unify_str(Rule::module, code)?;
  {
    let ast2 = module.ast.read().unwrap();
    let find_result2 = ast2.find_function_def(&MFArity::new_local_str("main", 0)).unwrap();
    let main_ty = module.unifier.infer_ast(find_result2).into_final_type();
    println!("{}: Inferred {} 🡆 {}", function_name!(), find_result2, main_ty);

    // Expected: Main -> number()|[atom1|atom2]
    let list_type = ErlType::List(Box::new(
      ErlType::union_of(vec![ErlType::atom_str("atom1"), ErlType::atom_str("atom2")], true)
      ));
    assert_eq!(main_ty,
               ErlType::union_of(vec![ErlType::Number, list_type], true),
               "Function main/0 must have inferred type: number()|[atom1|atom2]");
  }

  Ok(())
}
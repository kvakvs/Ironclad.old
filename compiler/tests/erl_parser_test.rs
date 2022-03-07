extern crate compiler;
extern crate function_name;

use std::ops::Deref;
use std::path::PathBuf;

use ::function_name::named;
use nom::Finish;

use compiler::erl_error::{ErlError, ErlResult};
use compiler::erlang::syntax_tree::erl_ast::ErlAst;
use compiler::erlang::syntax_tree::nom_parse::ErlParser;
use compiler::literal::Literal;
use compiler::project::module::Module;

mod test_util;

/// Try parse empty module
#[named]
#[test]
fn parse_empty_module() -> ErlResult<()> {
  test_util::start(function_name!(), "parse an empty module with start attribute only");
  let filename = PathBuf::from(function_name!());
  let code = format!("-module({}).\n", function_name!());
  let parsed = Module::from_module_source(&filename, &code)?;
  // let parsed = Module::parse_helper(&filename, &code, ErlParser::parse_module_attr)?;
  println!("Parsed empty module: «{}»\nAST: {}", code, &parsed.ast);
  Ok(())
}

/// Try parse `-export([])` attr
#[named]
#[test]
fn parse_export_attr() -> ErlResult<()> {
  test_util::start(function_name!(), "parse an export attr");

  let (_tail, pfna) = ErlParser::parse_funarity("name/123").unwrap();
  assert_eq!(pfna.name, "name");
  assert_eq!(pfna.arity, 123usize);

  let filename = PathBuf::from(function_name!());
  let code = format!("-module({}).
-export([module/2, format_error/1]).
", function_name!());
  let parsed = Module::from_module_source(&filename, &code)?;
  println!("Parsed module with export attr: «{}»\nAST: {}", code, &parsed.ast);
  Ok(())
}

/// Try parse empty module forms collection (from empty input)
#[named]
#[test]
fn parse_empty_module_forms_collection() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a whitespace only string as module forms collection");
  let code = "    \n   \r\n  ";
  let parse_result = ErlParser::parse_module_forms_collection(code);
  match parse_result.finish() {
    Ok((_tail, forms)) => {
      println!("Parsed empty module forms collection: «{}»\nResult: {:?}", code, forms)
    },
    Err(err) => return Err(ErlError::from_nom_error(code, err)),
  }
  Ok(())
}

/// Try parse module forms collection with 2 functions in it
#[named]
#[test]
fn parse_2_module_forms_collection() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a string with 2 function defs in it as module forms collection");
  let code = "fn1(A, B) -> A + B.\n  fn2(A) ->\n fn1(A, 4).";
  let parse_result = ErlParser::parse_module_forms_collection(code);
  match parse_result.finish() {
    Ok((_tail, forms)) => {
      println!("{} parsed: tail=«{}»\nResult={:?}", function_name!(), code, forms)
    },
    Err(err) => return Err(ErlError::from_nom_error(code, err)),
  }
  Ok(())
}

/// Try parse string
#[named]
#[test]
fn parse_string_test() -> ErlResult<()> {
  test_util::start(function_name!(), "parse a string literal");
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "\"abc\"").unwrap();

  if let ErlAst::Lit { value: lit, .. } = module.ast.deref() {
    if let Literal::String(value) = lit.deref() {
      assert_eq!(value, "abc");
      return Ok(());
    }
  }
  panic!("{} Expected: Literal(String) result, got {}", function_name!(), module.ast)
}

/// Try parse a 2+2 expression
#[named]
#[test]
fn parse_expr_2_plus_2() -> ErlResult<()> {
  let filename = PathBuf::from(function_name!());
  let expr_2 = Module::from_expr_source(&filename, " 2")?;
  println!("Parse \"2\": {}", expr_2.ast);
  assert!(matches!(expr_2.ast.deref(), ErlAst::Lit {..}));

  let expr_2_2 = Module::from_expr_source(&filename, " 2         + 2       ")?;
  println!("Parse \"2+2\": {}", expr_2_2.ast);
  assert!(matches!(expr_2_2.ast.deref(), ErlAst::BinaryOp { .. }));

  Ok(())
}

/// Try parse a flat + expression
#[named]
#[test]
fn parse_expr_flat() -> ErlResult<()> {
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "A + 123 + 333 + 6 + atom + Test")?;
  println!("Parse \"A+123+333+6+atom+Test\": {}", module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));
  Ok(())
}

/// Try parse a more complex expression
#[named]
#[test]
fn parse_expr_longer() -> ErlResult<()> {
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "123 + 1 / (2 * hello)")?;
  println!("Parse \"123+1/(2*hello)\": {}", module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));
  Ok(())
}

/// Try parse an expression with parentheses and division
#[named]
#[test]
fn parse_expr_2() -> ErlResult<()> {
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "(A +1)/ 2")?;
  println!("Parse \"(A+1)/2\": {}", module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));
  Ok(())
}

/// Try parse a comma expression with some simpler nested exprs
#[named]
#[test]
fn parse_expr_comma() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a comma separated list of expressions");
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "A, B, 123 * C")?;
  println!("Parse \"A,B,123*C\": {}", module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));

  Ok(())
}

/// Try parse a list and a tuple
#[named]
#[test]
fn parse_expr_containers() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a list and a tuple");
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "[1,2  ,3  ], {a, b ,C}")?;
  println!("Parse \"[1,2,3],{{a,b,C}}\": {}", module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));

  Ok(())
}

/// Try parse some function defs
#[named]
#[test]
fn parse_fn1() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a function returning some simple value");
  let filename = PathBuf::from(function_name!());
  let module = Module::from_fun_source(&filename, "f(A) -> atom123.")?;
  println!("Parse \"f(A) -> atom123.\": {}", module.ast);

  if let ErlAst::FnDef { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::FunctionDef, got {}", function_name!(), module.ast);
  }

  let func_count = if let Ok(scope_r) = module.scope.read() {
    scope_r.function_defs.len()
  } else {
    panic!()
  };
  assert_eq!(func_count, 1, "Module must have 1 function in its env");
  // assert_eq!(module1.function_clauses.len(), 1, "Module must have 1 function clause in its env");
  Ok(())
}

/// Try parse a function apply expr. This can be any expression immediately followed by
/// a parenthesized comma expression.
#[named]
#[test]
fn parse_apply_1() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a simple apply() expr");

  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "a_function()")?;
  println!("{}: parsed {}", function_name!(), module.ast);

  if let ErlAst::Apply { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::App, got {}", function_name!(), module.ast);
  }

  Ok(())
}

#[named]
#[test]
fn parse_apply_2() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse an apply() expression with a fancy left side");

  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "(123 + atom)()")?;
  println!("{}: parsed {}", function_name!(), module.ast);

  if let ErlAst::Apply { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::App, got {}", function_name!(), module.ast);
  }

  Ok(())
}

#[named]
#[test]
fn parse_apply_3() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a very fancy nested apply() expression");

  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "(F() + g())(test(), 123())")?;
  println!("{} parse_application 3 parsed {}", function_name!(), module.ast);

  if let ErlAst::Apply { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::App, got {}", function_name!(), module.ast);
  }
  Ok(())
}

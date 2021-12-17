extern crate compiler;
extern crate function_name;

use std::ops::Deref;

use ::function_name::named;

use compiler::erl_error::ErlResult;
use compiler::erlang::syntax_tree::erl_ast::ErlAst;
use compiler::literal::Literal;
use compiler::project::module::Module;

mod test_util;

/// Try parse string
#[named]
#[test]
fn parse_string_test() -> ErlResult<()> {
  let module = Module::from_expr_source("\"abc\"").unwrap();

  if let ErlAst::Lit { value: lit, .. } = module.ast.deref() {
    if let Literal::String(_value) = lit.deref() {
      return Ok(());
    }
  }
  panic!("{} Expected: Literal(String) result, got {}", function_name!(), module.ast)
}

/// Try parse a flat + expression
#[named]
#[test]
fn parse_expr_flat() -> ErlResult<()> {
  let module = Module::from_expr_source("A + 123 + 333 + 6 + atom + Test")?;

  if let ErlAst::BinaryOp { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::BinaryOp(+), got {}", function_name!(), module.ast);
  }

  Ok(())
}

/// Try parse a more complex expression
#[named]
#[test]
fn parse_expr_longer() -> ErlResult<()> {
  let module = Module::from_expr_source("123 + 1 / (2 * hello)")?;

  if let ErlAst::BinaryOp { .. } = module.ast.deref() {
    //ok
  } else {
    panic!("{} Expected: ErlAst::BinaryOp(+), got {}", function_name!(), module.ast);
  }
  Ok(())
}

/// Try parse a comma expression with some simpler nested exprs
#[named]
#[test]
fn parse_expr_comma() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a comma separated list of expressions");
  let module = Module::from_expr_source("A, B, 123 * C")?;

  if let ErlAst::BinaryOp(_loc, _expr) = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::BinaryOp with Comma, got {}", function_name!(), module.ast);
  }
  Ok(())
}

/// Try parse some function defs
#[named]
#[test]
fn parse_fn1() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a function returning some simple value");
  let module = Module::from_fun_source("f(A) -> atom123.")?;

  if let ErlAst::FnDef { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::FunctionDef, got {}", function_name!(), module.ast);
  }

  let func_count = if let Ok(registry) = module.registry.read() {
    registry.functions.len()
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
  let module = Module::from_expr_source("a_function()")?;
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
  let module = Module::from_expr_source("(123 + atom)()")?;
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
  let module = Module::from_expr_source("(F() + g())(test(), 123())")?;
  println!("{} parse_application 3 parsed {}", function_name!(), module.ast);

  if let ErlAst::Apply { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::App, got {}", function_name!(), module.ast);
  }
  Ok(())
}

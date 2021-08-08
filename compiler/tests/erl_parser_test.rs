extern crate compiler;
extern crate function_name;

mod test_util;

use ::function_name::named;
use compiler::syntaxtree::erl::erl_parser::{Rule};
use compiler::syntaxtree::erl::erl_ast::{ErlAst};
use compiler::syntaxtree::erl::node::literal_node::Literal;
use std::ops::Deref;
use compiler::erl_module::ErlModule;
use compiler::erl_error::ErlResult;

/// Try parse string
#[named]
#[test]
fn parse_string_test() -> ErlResult<()> {
  let mut module1 = ErlModule::default();
  module1.parse_str(Rule::string, "\"abc\"").unwrap();

  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::Lit(_loc, Literal::String(_value)) = ast.deref() {
      // ok
    } else {
      panic!("{} Expected: Literal(String) result, got {}", function_name!(), ast)
    }
  }
  Ok(())
}

/// Try parse a flat + expression
#[named]
#[test]
fn parse_expr_flat() -> ErlResult<()> {
  let mut module1 = ErlModule::default();
  module1.parse_str(Rule::expr, "A + 123 + 333 + 6 + atom + Test")?;

  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::BinaryOp { .. } = ast.deref() {
      // ok
    } else {
      panic!("{} Expected: ErlAst::BinaryOp(+), got {}", function_name!(), ast);
    }
  }
  Ok(())
}

/// Try parse a more complex expression
#[named]
#[test]
fn parse_expr_longer() -> ErlResult<()> {
  let mut module1 = ErlModule::default();
  module1.parse_str(Rule::expr, "123 + 1 / (2 * hello)")?;

  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::BinaryOp { .. } = ast.deref() {
      //ok
    } else {
      panic!("{} Expected: ErlAst::BinaryOp(+), got {}", function_name!(), ast);
    }
  }
  Ok(())
}

/// Try parse a comma expression with some simpler nested exprs
#[named]
#[test]
fn parse_expr_comma() -> ErlResult<()> {
  test_util::start(function_name!());
  let mut module1 = ErlModule::default();
  module1.parse_str(Rule::expr, "A, B, 123 * C")?;

  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::Comma { .. } = ast.deref() {
      // ok
    } else {
      panic!("{} Expected: ErlAst::Comma, got {}", function_name!(), ast);
    }
  }
  Ok(())
}

/// Try parse some function defs
#[named]
#[test]
fn parse_fn1() -> ErlResult<()> {
  test_util::start(function_name!());
  let mut module1 = ErlModule::default();
  module1.parse_str(Rule::function_def, "f(A) -> atom123.")?;
  {
    let ast = module1.ast.read().unwrap();
    if let ErlAst::FunctionDef { .. } = ast.deref() {
      // ok
    } else {
      panic!("{} Expected: ErlAst::FunctionDef, got {}", function_name!(), ast);
    }
  }
  assert_eq!(module1.env.functions.len(), 1, "Module must have 1 function in its env");
  assert_eq!(module1.env.function_clauses.len(), 1, "Module must have 1 function clause in its env");
  Ok(())
}

/// Try parse a function application expr. This can be any expression immediately followed by
/// a parenthesized comma expression.
#[named]
#[test]
fn parse_application1() -> ErlResult<()> {
  test_util::start(function_name!());
  let mut module1 = ErlModule::default();
  module1.parse_str(Rule::expr, "a_function()")?;
  println!("{}: parsed {}", function_name!(), module1.ast.read().unwrap());

  {
    let ast1 = module1.ast.read().unwrap();
    if let ErlAst::App { .. } = ast1.deref() {
      // ok
    } else {
      panic!("{} Expected: ErlAst::App, got {}", function_name!(), module1.ast.read().unwrap());
    }
  }
  Ok(())
}

#[named]
#[test]
fn parse_application2() -> ErlResult<()> {
  test_util::start(function_name!());
  let mut module2 = ErlModule::default();
  module2.parse_str(Rule::expr, "(123 + atom)()")?;
  println!("{}: parsed {}", function_name!(), module2.ast.read().unwrap());

  {
    let ast2 = module2.ast.read().unwrap();
    if let ErlAst::App { .. } = ast2.deref() {
      // ok
    } else {
      panic!("{} Expected: ErlAst::App, got {}", function_name!(), module2.ast.read().unwrap());
    }
  }
  Ok(())
}

#[named]
#[test]
fn parse_application3() -> ErlResult<()> {
  test_util::start(function_name!());
  let mut module3 = ErlModule::default();
  module3.parse_str(Rule::expr, "(F() + g())(test(), 123())")?;
  println!("{} parse_application 3 parsed {}", function_name!(), module3.ast.read().unwrap());

  {
    let ast3 = module3.ast.read().unwrap();
    if let ErlAst::App { .. } = ast3.deref() {
      // ok
    } else {
      panic!("{} Expected: ErlAst::App, got {}", function_name!(), module3.ast.read().unwrap());
    }
  }
  Ok(())
}

extern crate compiler;

mod test_util;

use compiler::syntaxtree::erl::erl_parser::{Rule};
use compiler::syntaxtree::erl::erl_ast::{ErlAst};
use compiler::syntaxtree::erl::node::literal_node::LiteralNode;
use std::ops::Deref;
use compiler::erl_module::ErlModule;
use compiler::erl_error::ErlResult;

/// Try parse string
#[test]
fn parse_string_test() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::string, "\"abc\"").unwrap();

  if let ErlAst::Lit(LiteralNode::String(_value)) = module1.ast.deref() {} else {
    assert!(false, "Expected: Literal(String) result, got {:?}", module1.ast)
  }
  Ok(())
}

/// Try parse a flat + expression
#[test]
fn parse_expr_flat() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::expr, "A + 123 + 333 + 6 + atom + Test")?;

  if let ErlAst::BinaryOp { .. } = module1.ast.deref() {} else {
    assert!(false, "Expected: ErlAst::BinaryOp(+), got {:?}", module1.ast);
  }
  Ok(())
}

/// Try parse a more complex expression
#[test]
fn parse_expr_longer() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::expr, "123 + 1 / (2 * hello)")?;

  if let ErlAst::BinaryOp { .. } = module1.ast.deref() {} else {
    assert!(false, "Expected: ErlAst::BinaryOp(+), got {:?}", module1.ast);
  }
  Ok(())
}

/// Try parse a comma expression with some simpler nested exprs
#[test]
fn parse_expr_comma() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::expr, "A, B, 123 * C")?;

  if let ErlAst::Comma { .. } = module1.ast.deref() {} else {
    assert!(false, "Expected: ErlAst::Comma, got {:?}", module1.ast);
  }
  Ok(())
}

/// Try parse some function defs
#[test]
fn parse_fn1() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::function_def, "f(A) -> atom123.")?;

  if let ErlAst::NewFunction { .. } = module1.ast.deref() {} else {
    assert!(false, "Expected: ErlAst::NewFunction, got {:?}", module1.ast);
  }
  Ok(())
}

/// Try parse a function application expr. This can be any expression immediately followed by
/// a parenthesized comma expression.
#[test]
fn parse_application() -> ErlResult<()> {
  let mut module1 = ErlModule::new_testing();
  module1.parse_str(Rule::expr, "a_function()")?;
  println!("parse_application 1 parsed {:?}", module1.ast);

  if let ErlAst::App { .. } = module1.ast.deref() {} else {
    assert!(false, "Expected: ErlAst::App, got {:?}", module1.ast);
  }

  let mut module2 = ErlModule::new_testing();
  module2.parse_str(Rule::expr, "(123 + atom)()")?;
  println!("parse_application 2 parsed {:?}", module2.ast);

  if let ErlAst::App { .. } = module2.ast.deref() {} else {
    assert!(false, "Expected: ErlAst::App, got {:?}", module2.ast);
  }

  let mut module3 = ErlModule::new_testing();
  module3.parse_str(Rule::expr, "(F() + g())(test(), 123())")?;
  println!("parse_application 3 parsed {:?}", module3.ast);

  if let ErlAst::App { .. } = module3.ast.deref() {} else {
    assert!(false, "Expected: ErlAst::App, got {:?}", module3.ast);
  }
  Ok(())
}
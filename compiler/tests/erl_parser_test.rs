extern crate compiler;

mod test_util;

use compiler::syntaxtree::erl::erl_parser::{Rule};
use compiler::syntaxtree::erl::erl_ast::{ErlAst};
use compiler::syntaxtree::erl::literal::ErlLit;
use std::ops::Deref;

/// Try parse string
#[test]
fn parse_string_test() {
  let s0 = test_util::erl_parse(Rule::string, "\"abc\"").unwrap();
  if let ErlAst::Lit(ErlLit::String(_value)) = s0.deref() {} else {
    assert!(false, "Expected: Literal(String) result, got {:?}", s0)
  }
}

/// Try parse a flat + expression
#[test]
fn parse_expr_flat() {
  let ex1 = test_util::erl_parse(Rule::expr, "A + 123 + 333 + 6 + atom + Test").unwrap();
  if let ErlAst::BinaryOp { .. } = ex1.deref() {} else {
    assert!(false, "Expected: ErlAst::BinaryOp(+), got {:?}", ex1);
  }
}

/// Try parse a more complex expression
#[test]
fn parse_expr_longer() {
  let ex1 = test_util::erl_parse(Rule::expr, "123 + 1 / (2 * hello)").unwrap();
  if let ErlAst::BinaryOp { .. } = ex1.deref() {} else {
    assert!(false, "Expected: ErlAst::BinaryOp(+), got {:?}", ex1);
  }
}

/// Try parse a comma expression with some simpler nested exprs
#[test]
fn parse_expr_comma() {
  let ex1 = test_util::erl_parse(Rule::expr, "A, B, 123 * C").unwrap();
  if let ErlAst::Comma { .. } = ex1.deref() {} else {
    assert!(false, "Expected: ErlAst::Comma, got {:?}", ex1);
  }
}

/// Try parse some function defs
#[test]
fn parse_fn1() {
  let f1 = test_util::erl_parse(Rule::function_def, "f(A) -> atom123.").unwrap();
  if let ErlAst::NewFunction { .. } = f1.deref() {} else {
    assert!(false, "Expected: ErlAst::NewFunction, got {:?}", f1);
  }
}

/// Try parse a function application expr.
/// Any expression followed by a () is an application. Even if invalid in the language semantic.
#[test]
fn parse_application() {
  let f1 = test_util::erl_parse(Rule::expr, "a_function()").unwrap();
  println!("parse_application f1 parsed {:?}", f1);
  if let ErlAst::App { .. } = f1.deref() {} else {
    assert!(false, "Expected: ErlAst::App, got {:?}", f1);
  }

  let f2 = test_util::erl_parse(Rule::expr, "(123 + atom)()").unwrap();
  println!("parse_application f2 parsed {:?}", f2);
  if let ErlAst::App { .. } = f2.deref() {} else {
    assert!(false, "Expected: ErlAst::App, got {:?}", f2);
  }

  let f3 = test_util::erl_parse(Rule::expr, "(F() + g())(test(), 123())").unwrap();
  println!("parse_application f3 parsed {:?}", f3);
  if let ErlAst::App { .. } = f3.deref() {} else {
    assert!(false, "Expected: ErlAst::App, got {:?}", f3);
  }
}
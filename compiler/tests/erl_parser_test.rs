extern crate compiler;
mod test_util;

use compiler::syntaxtree::erl::erl_parser::{Rule};
use compiler::syntaxtree::erl::erl_ast::{ErlAst};
use compiler::syntaxtree::erl::literal::ErlLiteral;
use std::ops::Deref;

/// Try parse string
#[test]
fn parse_string_test() {
  let s0 = test_util::erl_parse(Rule::string, "\"abc\"").unwrap();
  if let ErlAst::Lit(ErlLiteral::String(_value)) = s0.deref() {} else {
    assert!(false, "Expected: Literal(String) result, got {:?}", s0)
  }
}

/// Try parse a comma expression with some simpler nested exprs
#[test]
fn parse_expr1() {
  let ex1 = test_util::erl_parse(Rule::expr, "A, B, 123 + B").unwrap();
  if let ErlAst::Comma { .. } = ex1.deref() {} else {
    assert!(false, "Expected: ErlAst::Comma, got {:?}", ex1);
  }
}

/// Try parse some function defs
#[test]
fn parse_fn1() {
  let f1 = test_util::erl_parse(Rule::function_def, "f(A) -> ok.").unwrap();
  if let ErlAst::NewFunction{..} = f1.deref() {} else {
    assert!(false, "Expected: ErlAst::NewFunction, got {:?}", f1);
  }
}
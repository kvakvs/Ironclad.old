#![allow(clippy::upper_case_acronyms)]

#[derive(Parser)]
#[grammar = "syntaxtree/erl/erl_grammar.pest"]
pub struct ErlParser;

#[cfg(test)]
mod tests {
  use crate::syntaxtree::erl::erl_parser::{Rule};
  use crate::syntaxtree::test_util::erl_parse;
  use crate::syntaxtree::erl::erl_ast::{ErlAst};
  use crate::syntaxtree::erl::literal::ErlLiteral;
  use std::ops::Deref;

  #[test]
  /// Try parse string
  fn parse_string_test() {
    let s0 = erl_parse(Rule::string, "\"abc\"").unwrap();
    if let ErlAst::Lit(ErlLiteral::String(_value)) = s0.deref() {} else {
      assert!(false, "Expected: Literal(String) result, got {:?}", s0)
    }
  }

  #[test]
  /// Try parse a comma expression with some simpler nested exprs
  fn parse_expr1() {
    let ex1 = erl_parse(Rule::expr, "A, B, 123 + B").unwrap();
    if let ErlAst::Comma { .. } = ex1.deref() {} else {
      assert!(false, "Expected: ErlAst::Comma, got {:?}", ex1);
    }
  }

  /// Try parse some function defs
  fn parse_fn1() {
    let f1 = erl_parse(Rule::function_def, "f(A) -> ok.").unwrap();
    if let ErlAst::NewFunction{..} = f1.deref() {} else {
      assert!(false, "Expected: ErlAst::NewFunction, got {:?}", f1);
    }
  }
}
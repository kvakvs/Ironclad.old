#![allow(clippy::upper_case_acronyms)]

#[derive(Parser)]
#[grammar = "syntaxtree/erl/erl_grammar.pest"]
pub struct ErlParser;

#[cfg(test)]
mod tests {
  use crate::syntaxtree::erl::erl_parser::{Rule};
  // use pest::Parser;
  use crate::syntaxtree::test_util::erl_parse;
  use crate::syntaxtree::erl::erl_ast::{ErlAst, };

  #[test]
  /// Try parse string
  fn parse_string_test() {
    let s0 = erl_parse(Rule::string, "\"abc\"").unwrap();
    assert!(matches!(s0, ErlAst::String(_value)));
  }
}
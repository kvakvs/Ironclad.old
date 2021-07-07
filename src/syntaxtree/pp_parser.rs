#![allow(clippy::upper_case_acronyms)]

#[derive(Parser)]
#[grammar = "syntaxtree/pp_grammar.pest"]
pub struct PpParser;

#[cfg(test)]
mod tests {
  use crate::syntaxtree::pp_parser::{PpParser, Rule};
  use pest::Parser;
  use crate::syntaxtree::pp_ast::{PpAstTree, PpAst};
  use crate::erl_error::ErlResult;

  fn parse(rule: Rule, input: &str) -> ErlResult<PpAst> {
    let parse_output = PpParser::parse(rule, input)?.next().unwrap();
    PpAstTree::pp_parse_tokens_to_ast(parse_output)
  }

  #[test]
  /// Try s2_parse string
  fn parse_define0_test() {
    let define0 = parse(Rule::pp_define, "-define(AAA, true).\n").unwrap();
    assert!(matches!(define0, PpAst::Define(_name, _value)));
  }

  #[test]
  fn parse_include_test() {
    let inc1 = parse(Rule::pp_include, "-include (\"test\").\n").unwrap();
    assert_eq!(inc1, PpAst::Include("test".to_string()));

    let inc2 = parse(Rule::pp_include, "- include(\"test\"\n).\n").unwrap();
    assert_eq!(inc2, PpAst::Include("test".to_string()));

    let inc3 = parse(Rule::pp_include, "-include\n(\"test\"\n).\n").unwrap();
    assert_eq!(inc3, PpAst::Include("test".to_string()));
  }

  #[test]
  fn parse_define_test() {
    let d0 = parse(Rule::pp_define, "- define(AAA, \"aaa\").\n").unwrap();
    assert!(matches!(d0, PpAst::Define(_name, _value)), "Parsing define(AAA) failed");

    let d1 = parse(Rule::pp_define, "-define(BBB, 666)\n.\n").unwrap();
    assert!(matches!(d1, PpAst::Define(_name, _value)), "Parsing define(BBB) failed");
  }

  #[test]
  fn parse_define_fun_test() {
    let d0 = parse(Rule::pp_define_fun, "-define(AAA(X,Y), \"aaa\").\n").unwrap();
    assert!(matches!(d0, PpAst::DefineFun{ name: _name, args: _args, body: _b}));
  }
}
#![allow(clippy::upper_case_acronyms)]
#[derive(Parser)]
#[grammar = "erl_parse/erl_preprocess.pest"]
pub struct ErlPreprocessorParser;

#[cfg(test)]
mod tests {
  use crate::erl_parse::erl_pp::{ErlPreprocessorParser, Rule};
  use pest::Parser;

  fn check_if_parses(rule: Rule, input: &str, inner_output: &str) {
    match ErlPreprocessorParser::parse(rule, input) {
      Ok(mut result) => {
        assert_eq!(result.next().unwrap().into_inner().as_str(),
                   inner_output,
                   "Parsing a rule {:?} from {} did not work", rule, input);
      }
      Err(e) =>
        assert!(false, "Parse error at{}", e)
    }
  }

  #[test]
  /// Try parse string
  fn parse_string_test() {
    check_if_parses(Rule::string, "\"test\"", "test");
  }
  fn parse_include_test() {
    check_if_parses(Rule::pp_include, "\n-include(\"test\").\n", "test");
    check_if_parses(Rule::pp_include, "\n-include(\"test\"\n).\n", "test");
    check_if_parses(Rule::pp_include, "-include\n(\"test\"\n).\n", "test");
  }
}
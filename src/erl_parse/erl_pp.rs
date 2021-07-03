#[derive(Parser)]
#[grammar = "erl_parse/erl_preprocess.pest"]
pub struct ErlPreprocessorParser;

#[cfg(test)]
mod tests {
  use crate::erl_parse::erl_pp::{ErlPreprocessorParser, Rule};
  use pest::Parser;

  #[test]
  /// Try parse string
  fn parse_string_test() {
    let sample = "\"test\"";
    let mut result = ErlPreprocessorParser::parse(Rule::string, sample).unwrap();
    assert_eq!(result.next().unwrap().into_inner().as_str(), sample);
  }
}
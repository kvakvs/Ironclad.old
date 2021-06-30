use crate::erl_error::{ErlResult};
use crate::erl_parse::erl_pp::{ErlPreprocessorParser, Rule};
use crate::erl_parse::pp_ast::{PpAstNode, PpAstTree};
use crate::erl_parse::{helpers, Span};
use crate::project::source_file::SourceFile;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::path::Path;
use std::sync::Arc;

impl PpAstTree {
  /// Does rough preparse of ERL files, only being interested in -include, -ifdef, macros, ... etc
  ///
  /// -define(Name(...), ...).
  /// -if(Bool), -ifdef(Macro), -ifndef(Macro), -undef(Macro), -else, -elif(Bool), -endif
  /// -error(Term), -warning(Term) (OTP 19+)
  /// ?MODULE, ?MODULE_STRING, ?FILE, ?LINE, ?MACHINE='BEAM', ?FUNCTION_NAME, ?FUNCTION_ARITY,
  /// ?OTP_RELEASE (OTP 21+)
  /// ??MACRO to stringify the tokens in the macro argument
  ///
  /// Return: Parsed preprocessor forms list (directives, and text fragments and comments)
  /// Lifetime note: Parse input string must live at least as long as parse tree is alive
  pub fn from_source_file(source_file: &Arc<SourceFile>) -> ErlResult<PpAstTree> {
    let successful_parse = ErlPreprocessorParser::parse(Rule::file, &source_file.text)?;
    println!("OK => {:?}", successful_parse);

    let pp_tree = PpAstTree {
      source: source_file.clone(),
      nodes: Self::parse_pp_ast_tree(successful_parse),
    };
    Ok(pp_tree)
  }

  fn parse_pp_ast_tree(pairs: Pairs<Rule>) -> Vec<PpAstNode> {
    pairs.map(|pair| {
      let mut inner_rules = pair.into_inner();
      // let name = inner_rules
      //     .next()
      //     .unwrap()
      //     .into_inner()
      //     .next()
      //     .unwrap()
      //     .as_str();
      Self::parse_pp_ast(inner_rules.next().unwrap())
    }).collect::<Vec<PpAstNode>>()
  }

  fn parse_pp_ast(pair: Pair<Rule>) -> PpAstNode {
    match pair.as_rule() {
      Rule::text => {
        PpAstNode::Text(String::from(pair.as_str()))
      }
      // Rule::file =>
      //   pair.into_inner()
      //       .map(|pair| {
      //         let mut inner_rules = pair.into_inner();
      //         let name = inner_rules
      //             .next()
      //             .unwrap()
      //             .into_inner()
      //             .next()
      //             .unwrap()
      //             .as_str();
      //         let value = Self::parse_pp_ast(inner_rules.next().unwrap());
      //         (name, value)
      //       })
      //       .collect(),
      // Rule::array => JSONValue::Array(pair.into_inner().map(parse_value).collect()),
      // Rule::string => JSONValue::String(pair.into_inner().next().unwrap().as_str()),
      // Rule::number => JSONValue::Number(pair.as_str().parse().unwrap()),
      // Rule::boolean => JSONValue::Boolean(pair.as_str().parse().unwrap()),
      // Rule::null => JSONValue::Null,
      other => unreachable!("value: {:?}", other),
    }
  }
}
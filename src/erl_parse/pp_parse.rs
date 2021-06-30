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

    let pp_tree = PpAstTree {
      source: source_file.clone(),
      nodes: Self::parse_pp_ast_tree(successful_parse),
    };
    pp_tree.nodes.iter().for_each(|n| println!("Node: {:?}", n));
    Ok(pp_tree)
  }

  fn parse_pp_ast_tree(pairs: Pairs<Rule>) -> Vec<PpAstNode> {
    pairs.map(|pair| {
      let mut inner_rules = pair.into_inner();
      Self::parse_pp_ast(inner_rules.next().unwrap())
    }).collect::<Vec<PpAstNode>>()
  }

  /// Convert a node produced by the Pest PEG parser into AST node
  fn parse_pp_ast(pair: Pair<Rule>) -> PpAstNode {
    match pair.as_rule() {
      Rule::file => {
        // Parse all nested file elements, comments and text fragments
        let ast_nodes = pair.into_inner().map(Self::parse_pp_ast).collect::<Vec<PpAstNode>>();
        PpAstNode::File(ast_nodes)
      }
      Rule::text => PpAstNode::Text(String::from(pair.as_str())),

      Rule::pp_directive_include => PpAstNode::Include(pair.into_inner().to_string()),

      Rule::pp_directive_include_lib => PpAstNode::IncludeLib(pair.into_inner().to_string()),

      Rule::pp_directive => {
        let mut inner_rules = pair.into_inner();
        let ident = inner_rules.next().unwrap();
        let args = inner_rules.map(Self::parse_pp_ast).collect::<Vec<PpAstNode>>();
        PpAstNode::Attr {
          name: String::from(ident.as_str()),
          args
        }
      }

      Rule::pp_directive_arg => PpAstNode::Text(String::from(pair.as_str())),

      other => unreachable!("value: {:?}", other),
    }
  }
}
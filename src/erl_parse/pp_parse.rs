use crate::erl_error::{ErlResult};
use crate::erl_parse::erl_pp::{ErlPreprocessorParser, Rule};
use crate::erl_parse::pp_ast::{PpAstNode, PpAstTree};
use crate::project::source_file::SourceFile;
use pest::iterators::{Pair};
use pest::Parser;
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
    let successful_parse = ErlPreprocessorParser::parse(Rule::file, &source_file.text)?.next().unwrap();
    match Self::parse_pp_ast(successful_parse) {
      PpAstNode::File(nodes) => {
        let pp_tree = PpAstTree {
          source: source_file.clone(),
          nodes,
        };

        // pp_tree.nodes.iter().for_each(|n| println!("Node: {:?}", n));
        Ok(pp_tree)
      }
      _ => panic!("Only File() AST node is expected as parse result root")
    }
  }

  /// Convert a parse node produced by the Pest PEG parser into Preprocessor AST node
  fn parse_pp_ast(pair: Pair<Rule>) -> PpAstNode {
    match pair.as_rule() {
      Rule::file => {
        // Parse all nested file elements, comments and text fragments
        let ast_nodes = pair.into_inner().map(Self::parse_pp_ast).collect::<Vec<PpAstNode>>();
        PpAstNode::File(ast_nodes)
      }

      Rule::text => PpAstNode::Text(String::from(pair.as_str())),

      // Rule::pp_module => PpAstNode::Module(String::from(pair.into_inner().as_str())),

      Rule::pp_include => PpAstNode::Include(String::from(pair.into_inner().as_str())),

      Rule::pp_include_lib => PpAstNode::IncludeLib(String::from(pair.into_inner().as_str())),

      Rule::pp_ifdef => PpAstNode::Ifdef(String::from(pair.as_str())),
      Rule::pp_ifndef => PpAstNode::Ifndef(String::from(pair.as_str())),
      Rule::pp_if => PpAstNode::If(String::from(pair.as_str())),
      Rule::pp_elif => PpAstNode::Elif(String::from(pair.as_str())),
      Rule::pp_else => PpAstNode::Else,
      Rule::pp_endif => PpAstNode::Endif,

      Rule::pp_define => {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let body = String::from(inner.next().unwrap().as_str());
        PpAstNode::Define(name, body)
      },

      // Rule::pp_generic => {
      //   let mut inner_rules = pair.into_inner();
      //   let ident = inner_rules.next().unwrap();
      //   let args = inner_rules.map(Self::parse_pp_ast).collect::<Vec<PpAstNode>>();
      //   PpAstNode::Attr {
      //     name: String::from(ident.as_str()),
      //     args,
      //   }
      // }

      // Rule::pp_generic_arg => PpAstNode::Text(String::from(pair.as_str())),
      Rule::COMMENT => PpAstNode::Comment(String::from(pair.as_str())),

      other => unreachable!("value: {:?}", other),
    }
  }
}
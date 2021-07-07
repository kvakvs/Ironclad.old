use crate::erl_error::{ErlResult, ErlError};
use crate::syntaxtree::pp_parser::{PpParser, Rule};
use crate::syntaxtree::pp_ast::{PpAst, PpAstTree};
use crate::project::source_file::SourceFile;
use pest::iterators::{Pair};
use pest::Parser;
use std::sync::Arc;
use crate::syntaxtree::pp_expr::PpExpr;

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
  /// Lifetime note: Parse input string must live at least as long as s2_parse tree is alive
  pub fn from_source_file(source_file: &Arc<SourceFile>) -> ErlResult<PpAstTree> {
    let successful_parse = PpParser::parse(Rule::file, &source_file.text)?.next().unwrap();

    let mut pp_tree = PpAstTree {
      source: source_file.clone(),
      nodes: vec![],
    };

    match pp_tree.pp_parse_tokens_to_ast(successful_parse) {
      Ok(PpAst::File(nodes)) => {
        // pp_tree.nodes.iter().for_each(|n| println!("Node: {:?}", n));
        pp_tree.nodes = nodes;
        Ok(pp_tree)
      }
      _ => panic!("Only File() AST node is expected as s2_parse result root")
    }
  }

  fn string_from(pair: Pair<Rule>) -> ErlResult<String> {
    Ok(String::from(pair.into_inner().as_str()))
  }

  fn string_from_quoted_string(pair: Pair<Rule>) -> ErlResult<String> {
    Self::string_from(pair.into_inner().next().unwrap())
  }

  fn parse_expr(&self, pair: Pair<Rule>) -> ErlResult<PpExpr> {
    // String::from(pair.into_inner().as_str())
    match self.pp_parse_tokens_to_ast(pair) {
      Ok(PpAst::Expr(out)) => Ok(out),
      _ => ErlError::pp_parse(&self.source.file_name,"Expression expected")
    }
  }

  /// Convert a s2_parse node produced by the Pest PEG parser into Preprocessor AST node
  pub fn pp_parse_tokens_to_ast(&self, pair: Pair<Rule>) -> ErlResult<PpAst> {
    let result = match pair.as_rule() {
      Rule::file => {
        // Parse all nested file elements, comments and text fragments
        let ast_nodes = pair.into_inner()
            .map(|p| self.pp_parse_tokens_to_ast(p))
            .map(|r| r.unwrap())
            .collect::<Vec<PpAst>>();
        PpAst::File(ast_nodes)
      }

      Rule::text => PpAst::Text(String::from(pair.as_str())),

      Rule::pp_include => PpAst::Include(Self::string_from_quoted_string(pair)?),

      Rule::pp_include_lib => PpAst::IncludeLib(Self::string_from_quoted_string(pair)?),

      Rule::pp_ifdef => PpAst::Ifdef(Self::string_from(pair)?),
      Rule::pp_ifndef => PpAst::Ifndef(Self::string_from(pair)?),

      // -if and -elif can have boolean expressions in them
      Rule::pp_if => PpAst::If(self.parse_expr(pair)?),
      Rule::pp_elif => PpAst::Elif(self.parse_expr(pair)?),

      Rule::pp_else => PpAst::Else,
      Rule::pp_endif => PpAst::Endif,

      Rule::pp_error => PpAst::Error(String::from(pair.into_inner().as_str())),
      Rule::pp_warning => PpAst::Warning(String::from(pair.into_inner().as_str())),

      Rule::pp_define => {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let body = String::from(inner.next().unwrap().as_str());
        PpAst::Define(name, body)
      },

      Rule::pp_define_fun => {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let args = inner.next().unwrap().into_inner()
            .into_iter()
            .map(|n| String::from(n.as_str()))
            .collect();
        let body = String::from(inner.next().unwrap().as_str());
        PpAst::DefineFun{name, args, body}
      },

      Rule::COMMENT => PpAst::Comment(String::from(pair.as_str())),

      other => unreachable!("value: {:?}", other),
    };
    Ok(result)
  }
}

#[cfg(test)]
mod tests {
  // fn parse(rule: Rule, input: &str) -> ErlResult<PpAst> {
  //   let parse_output = ErlPreprocessorParser::parse(rule, input)?.next().unwrap();
  //   PpAstTree::pp_parse_tokens_to_ast(parse_output)
  // }
}
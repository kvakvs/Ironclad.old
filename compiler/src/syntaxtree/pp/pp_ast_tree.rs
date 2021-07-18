//! Erlang preprocessor AST tree
use crate::syntaxtree::pp::pp_parser::{PpParser, Rule};
use crate::syntaxtree::pp::pp_ast::{PpAst, PpAstTree};
use crate::project::source_file::SourceFile;
use pest::iterators::{Pair};
use pest::Parser;
use std::sync::Arc;
use crate::erl_error::ErlResult;
use std::rc::Rc;

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
      nodes: Rc::new(PpAst::Empty),
    };

    match pp_tree.pp_parse_tokens_to_ast(successful_parse) {
      Ok(root) => {
        pp_tree.nodes = root;
        Ok(pp_tree)
      }
      _ => panic!("Only 'file' AST node is expected as pp_ast_tree parse result root")
    }
  }

  fn string_from(pair: Pair<Rule>) -> ErlResult<String> {
    Ok(String::from(pair.into_inner().as_str()))
  }

  fn string_from_quoted_string(pair: Pair<Rule>) -> ErlResult<String> {
    Self::string_from(pair.into_inner().next().unwrap())
  }

  // fn parse_expr(&self, pair: Pair<Rule>) -> ErlResult<PpExpr> {
  //   // String::from(pair.into_inner().as_str())
  //   match self.pp_parse_tokens_to_ast(pair) {
  //     Ok(PpAst::Expr(out)) => Ok(out),
  //     _ => ErlError::pp_parse(&self.source.file_name,"Expression expected")
  //   }
  // }

  /// Convert Pest syntax token tree produced by the Pest PEG parser into Preprocessor AST tree
  pub fn pp_parse_tokens_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Rc<PpAst>> {
    let result: Rc<PpAst> = match pair.as_rule() {
      Rule::file => {
        // Parse all nested file elements, comments and text fragments
        let ast_nodes = pair.into_inner()
            .map(|p| self.pp_parse_tokens_to_ast(p))
            .map(Result::unwrap)
            .collect::<Vec<Rc<PpAst>>>();
        Rc::new(PpAst::File(ast_nodes))
      }

      Rule::text => {
        Rc::new(PpAst::Text(String::from(pair.as_str())))
      },

      Rule::pp_include => {
        Rc::new(PpAst::Include(Self::string_from_quoted_string(pair)?))
      },

      Rule::pp_include_lib => {
        Rc::new(PpAst::IncludeLib(Self::string_from_quoted_string(pair)?))
      },

      Rule::pp_ifdef => {
        Rc::new(PpAst::Ifdef(Self::string_from(pair)?))
      },
      Rule::pp_ifndef => {
        Rc::new(PpAst::Ifndef(Self::string_from(pair)?))
      },

      // -if and -elif can have boolean expressions in them
      Rule::pp_if => {
        Rc::new(PpAst::If(Self::string_from(pair)?))
      },
      Rule::pp_elif => {
        Rc::new(PpAst::Elif(Self::string_from(pair)?))
      },

      Rule::pp_else => Rc::new(PpAst::Else),
      Rule::pp_endif => Rc::new(PpAst::Endif),

      Rule::pp_error => {
        Rc::new(PpAst::Error(String::from(pair.into_inner().as_str())))
      },
      Rule::pp_warning => {
        Rc::new(PpAst::Warning(String::from(pair.into_inner().as_str())))
      },

      Rule::pp_define => {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let body = String::from(inner.next().unwrap().as_str());
        Rc::new(PpAst::Define(name, body))
      }

      Rule::pp_define_fun => {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let args = inner.next().unwrap().into_inner()
            .into_iter()
            .map(|n| String::from(n.as_str()))
            .collect();
        let body = String::from(inner.next().unwrap().as_str());
        Rc::new(PpAst::DefineFun { name, args, body })
      }

      Rule::COMMENT => {
        Rc::new(PpAst::Comment(String::from(pair.as_str())))
      },

      other => unreachable!("PpAst value: {:?}", other),
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
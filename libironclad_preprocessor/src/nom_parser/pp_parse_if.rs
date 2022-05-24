//! Parsing tools for `-if` family of directives

use crate::nom_parser::pp_parse_types::{
  PpAstParserResult, StrSliceParserResult, VecPpAstParserResult,
};
use crate::nom_parser::PreprocessorParser;
use crate::syntax_tree::pp_ast::PpAst;
use libironclad_erlang::syntax_tree::nom_parse::misc::ws_before;
use libironclad_erlang::syntax_tree::nom_parse::ErlParser;
use nom::character::complete::char;
use nom::combinator::{cut, map, opt, recognize};
use nom::{combinator, multi, sequence};
use std::ops::Deref;
use std::sync::Arc;

impl PreprocessorParser {
  /// Parses multiple lines of any directives except `-endif.` or `-else.`
  fn parse_if_true_block_till_else(input: &str) -> VecPpAstParserResult {
    multi::many0(combinator::verify(Self::parse_fragment, |frag: &Arc<PpAst>| {
      !frag.is_else() && !frag.is_elseif() && !frag.is_endif()
    }))(input)
  }

  /// Parse a `-if(EXPR).` `<LINES>` then optional `-else. <LINES> -endif.`
  pub fn parse_if_block(input: &str) -> PpAstParserResult {
    map(
      sequence::tuple((
        ws_before(Self::parse_if_temporary),
        // Consume lines and directives until an `-else` or `-endif`
        Self::parse_if_true_block_till_else,
        // Optional -else. <LINES> block
        cut(opt(sequence::preceded(
          Self::parse_else_temporary,
          multi::many0(Self::parse_fragment),
        ))),
        cut(Self::consume_endif),
      )),
      |(pp_if_expr, branch_true, branch_false, _endif)| {
        if let PpAst::_TemporaryIf(if_expr) = pp_if_expr.deref() {
          PpAst::new_if(if_expr.clone(), Some(branch_true), branch_false)
        } else {
          unreachable!("This code path should not execute")
        }
      },
    )(input)
  }

  /// Parse a `-if(EXPR)` and return a temporary node
  pub fn parse_if_temporary(input: &str) -> PpAstParserResult {
    map(
      sequence::preceded(
        Self::match_dash_tag("if"),
        sequence::delimited(
          ws_before(char('(')),
          ws_before(ErlParser::parse_expr),
          ws_before(char(')')),
        ),
      ),
      PpAst::new_if_temporary,
    )(input)
  }

  /// Parse a `-elif(EXPR)` into a temporary AST node
  pub(crate) fn parse_elif_temporary(input: &str) -> PpAstParserResult {
    map(
      sequence::preceded(
        Self::match_dash_tag("elif"),
        sequence::delimited(
          ws_before(char('(')),
          ws_before(ErlParser::parse_expr),
          ws_before(char(')')),
        ),
      ),
      PpAst::new_elif_temporary,
    )(input)
  }

  /// Parse a `-ifdef(MACRO_NAME)`
  pub(crate) fn parse_ifdef_temporary(input: &str) -> PpAstParserResult {
    map(
      sequence::preceded(
        Self::match_dash_tag("ifdef"),
        sequence::delimited(
          ws_before(char('(')),
          ws_before(Self::parse_macro_ident),
          ws_before(char(')')),
        ),
      ),
      PpAst::new_ifdef_temporary,
    )(input)
  }

  /// Parse a `-ifndef(MACRO_NAME)`
  pub fn parse_ifndef_temporary(input: &str) -> PpAstParserResult {
    map(
      sequence::preceded(
        Self::match_dash_tag("ifndef"),
        sequence::delimited(
          ws_before(char('(')),
          ws_before(Self::parse_macro_ident),
          ws_before(char(')')),
        ),
      ),
      PpAst::new_ifndef_temporary,
    )(input)
  }

  /// Parse a `-else.`, return a temporary `Else` node, which will not go into final `PpAst`
  pub fn parse_else_temporary(input: &str) -> PpAstParserResult {
    map(
      sequence::terminated(
        sequence::preceded(
          Self::match_dash_tag("else"),
          opt(sequence::pair(ws_before(char('(')), ws_before(char(')')))),
        ),
        Self::dot_newline,
      ),
      |_opt| PpAst::_TemporaryElse.into(),
    )(input)
  }

  fn maybe_empty_parens(input: &str) -> StrSliceParserResult {
    recognize(opt(sequence::pair(ws_before(char('(')), ws_before(char(')')))))(input)
  }

  /// Parse a `-endif.`, return a temporary `Endif` node, which will not go into final `PpAst`
  pub fn parse_endif_temporary(input: &str) -> PpAstParserResult {
    map(
      sequence::delimited(
        Self::match_dash_tag("endif"),
        Self::maybe_empty_parens,
        Self::dot_newline,
      ),
      |_opt| PpAst::_TemporaryEndif.into(),
    )(input)
  }

  /// Parse a `-endif.` and return it as a `&str` slice
  fn consume_endif(input: &str) -> StrSliceParserResult {
    recognize(sequence::preceded(
      Self::match_dash_tag("endif"),
      opt(sequence::pair(ws_before(char('(')), ws_before(char(')')))),
    ))(input)
  }
}

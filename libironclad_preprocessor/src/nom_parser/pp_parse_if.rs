//! Parsing tools for `-if` family of directives

use crate::nom_parser::pp_parse_types::{PpAstParserResult, StrSliceParserResult};
use crate::nom_parser::PreprocessorParser;
use crate::syntax_tree::pp_ast::PpAst;
use libironclad_erlang::syntax_tree::nom_parse::misc::MiscParser;
use libironclad_erlang::syntax_tree::nom_parse::ErlParser;
use nom::character::complete::char;
use nom::{bytes::complete::tag, combinator, multi, sequence};
use std::ops::Deref;
use std::sync::Arc;

impl PreprocessorParser {
  /// Parse a `-if(EXPR)` and return a temporary node
  fn parse_if_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("if")),
        sequence::delimited(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(ErlParser::parse_expr),
          MiscParser::ws_before(char(')')),
        ),
      ),
      PpAst::new_if_temporary,
    )(input)
  }

  /// Parse a `-if(EXPR).` `<LINES>` then optional `-else. <LINES> -endif.`
  pub(crate) fn parse_if_block(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::tuple((
        MiscParser::ws_before(Self::parse_if_temporary),
        // Consume lines and directives until an `-else` or `-endif`
        multi::many0(combinator::verify(Self::parse_fragment, |frag: &Arc<PpAst>| {
          !frag.is_else() && !frag.is_elseif()
        })),
        // Optional -else. <LINES> block
        combinator::opt(sequence::preceded(
          Self::parse_else_temporary,
          multi::many0(Self::parse_fragment),
        )),
        Self::consume_endif,
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

  /// Parse a `-elif(EXPR)` into a temporary AST node
  pub(crate) fn parse_elif_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("elif")),
        sequence::delimited(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(ErlParser::parse_expr),
          MiscParser::ws_before(char(')')),
        ),
      ),
      PpAst::new_elif_temporary,
    )(input)
  }

  /// Parse a `-ifdef(MACRO_NAME)`
  pub(crate) fn parse_ifdef_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("ifdef")),
        sequence::delimited(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(Self::parse_macro_ident),
          MiscParser::ws_before(char(')')),
        ),
      ),
      PpAst::new_ifdef_temporary,
    )(input)
  }

  /// Parse a `-ifndef(MACRO_NAME)`
  pub(crate) fn parse_ifndef_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("ifndef")),
        sequence::delimited(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(Self::parse_macro_ident),
          MiscParser::ws_before(char(')')),
        ),
      ),
      PpAst::new_ifndef_temporary,
    )(input)
  }

  /// Parse a `-else.`, return a temporary `Else` node, which will not go into final `PpAst`
  pub(crate) fn parse_else_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("else")),
        combinator::opt(sequence::pair(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(char(')')),
        )),
      ),
      |_opt| PpAst::_TemporaryElse.into(),
    )(input)
  }

  /// Parse a `-endif.` and return it as a `&str` slice
  fn consume_endif(input: &str) -> StrSliceParserResult {
    combinator::recognize(sequence::preceded(
      MiscParser::ws_before(tag("endif")),
      combinator::opt(sequence::pair(
        MiscParser::ws_before(char('(')),
        MiscParser::ws_before(char(')')),
      )),
    ))(input)
  }
}

//! Parsing tools for `-if` family of directives

use crate::nom_parser::pp_parse_types::{
  PpAstParserResult, StrSliceParserResult, VecPpAstParserResult,
};
use crate::nom_parser::PreprocessorParser;
use crate::syntax_tree::pp_ast::PpAst;
use libironclad_erlang::syntax_tree::nom_parse::misc::{par_close, par_open, ws_before};
use libironclad_erlang::syntax_tree::nom_parse::ErlParser;
use nom::combinator::{map, opt, recognize, verify};
use nom::error::context;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use std::ops::Deref;
use std::sync::Arc;

impl PreprocessorParser {
  /// Parses multiple lines of any directives except `-endif.` or `-else.`
  fn parse_fragments_till_else(input: &str) -> VecPpAstParserResult {
    many0(verify(Self::parse_fragment, |frag: &Arc<PpAst>| {
      !frag.is_else() && !frag.is_elseif() && !frag.is_endif()
    }))(input)
  }

  /// Parses multiple lines of any directives except `-endif.`
  fn parse_fragments_till_endif(input: &str) -> VecPpAstParserResult {
    many0(verify(Self::parse_fragment, |frag: &Arc<PpAst>| !frag.is_endif()))(input)
  }

  /// Parse a `-if(EXPR).` `<LINES>` then optional `-else. <LINES> -endif.`
  pub fn if_directive(input: &str) -> PpAstParserResult {
    map(
      terminated(
        tuple((
          Self::parse_if_temporary,
          // Consume lines and directives until an `-else` or `-endif`
          context("Condition true section of a preprocessor if", Self::parse_fragments_till_else),
          // Optional -else. <LINES> block
          context(
            "Condition false section of a preprocessor if",
            opt(preceded(Self::else_temporary_directive, Self::parse_fragments_till_endif)),
          ),
        )),
        // Ending with an endif
        Self::endif_temporary_directive,
      ),
      |(pp_if_expr, branch_true, branch_false)| {
        if let PpAst::_TemporaryIf(if_expr) = pp_if_expr.deref() {
          let branch_true1 = if branch_true.is_empty() { None } else { Some(branch_true) };
          PpAst::new_if(if_expr.clone(), branch_true1, branch_false)
        } else {
          unreachable!("This code path should not execute")
        }
      },
    )(input)
  }

  /// Parse a `-if(EXPR)` and return a temporary node
  pub fn parse_if_temporary(input: &str) -> PpAstParserResult {
    map(
      preceded(
        Self::match_dash_tag("if"),
        delimited(par_open, ws_before(ErlParser::parse_expr), par_close),
      ),
      PpAst::new_if_temporary, // Builds a temporary If node with erl expression in it
    )(input)
  }

  /// Parse a `-elif(EXPR)` into a temporary AST node
  pub(crate) fn elif_temporary_directive(input: &str) -> PpAstParserResult {
    map(
      preceded(
        Self::match_dash_tag("elif"),
        delimited(par_open, ws_before(ErlParser::parse_expr), par_close),
      ),
      PpAst::new_elif_temporary,
    )(input)
  }

  /// Parse a `-ifdef(MACRO_NAME)`
  pub(crate) fn ifdef_temporary_directive(input: &str) -> PpAstParserResult {
    map(
      preceded(
        Self::match_dash_tag("ifdef"),
        delimited(par_open, ws_before(Self::macro_ident), par_close),
      ),
      PpAst::new_ifdef_temporary,
    )(input)
  }

  /// Parse a `-ifndef(MACRO_NAME)`
  pub fn ifndef_temporary_directive(input: &str) -> PpAstParserResult {
    map(
      preceded(
        Self::match_dash_tag("ifndef"),
        delimited(par_open, ws_before(Self::macro_ident), par_close),
      ),
      PpAst::new_ifndef_temporary,
    )(input)
  }

  /// Parse a `-else.`, return a temporary `Else` node, which will not go into final `PpAst`
  pub fn else_temporary_directive(input: &str) -> PpAstParserResult {
    map(
      terminated(
        preceded(Self::match_dash_tag("else"), opt(pair(par_open, par_close))),
        Self::dot_newline,
      ),
      |_opt| PpAst::_TemporaryElse.into(),
    )(input)
  }

  fn maybe_empty_parens(input: &str) -> StrSliceParserResult {
    recognize(opt(pair(par_open, par_close)))(input)
  }

  /// Parse a `-endif.`, return a temporary `Endif` node, which will not go into final `PpAst`
  pub fn endif_temporary_directive(input: &str) -> PpAstParserResult {
    map(
      delimited(Self::match_dash_tag("endif"), Self::maybe_empty_parens, Self::dot_newline),
      |_opt| PpAst::_TemporaryEndif.into(),
    )(input)
  }
}

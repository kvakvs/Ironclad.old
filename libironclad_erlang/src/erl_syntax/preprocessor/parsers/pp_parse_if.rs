//! Parsing tools for `-if` family of directives

use crate::erl_syntax::erl_ast::{ErlAst, ErlAstType};
use crate::erl_syntax::parsers::misc::{
  match_dash_tag, par_close, par_open, period_newline, ws_before,
};
use crate::erl_syntax::parsers::{
  AstParserResult, ErlParser, StrSliceParserResult, VecAstParserResult,
};
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType::{_TemporaryElse, _TemporaryEndif};
use crate::erl_syntax::preprocessor::parsers::preprocessor_parser::PreprocessorParser;
use libironclad_error::source_loc::SourceLoc;
use nom::combinator::{map, opt, recognize, verify};
use nom::error::context;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use std::sync::Arc;

impl PreprocessorParser {
  /// Parses multiple lines of any directives except `-endif.` or `-else.`
  fn parse_fragments_till_else(input: &str) -> VecAstParserResult {
    many0(verify(ErlParser::module_form, |frag: &Arc<ErlAst>| {
      !frag.is_else() && !frag.is_elseif() && !frag.is_endif()
    }))(input)
  }

  /// Parses multiple lines of any directives except `-endif.`
  fn parse_fragments_till_endif(input: &str) -> VecAstParserResult {
    many0(verify(ErlParser::module_form, |frag: &Arc<ErlAst>| !frag.is_endif()))(input)
  }

  /// Parse a `-if(EXPR).` `<LINES>` then optional `-else. <LINES> -endif.`
  pub fn if_block(input: &str) -> AstParserResult {
    map(
      terminated(
        tuple((
          Self::if_directive,
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
        if let ErlAstType::Preprocessor(PreprocessorNodeType::_TemporaryIf(if_expr)) =
          &pp_if_expr.content
        {
          let branch_true1 = if branch_true.is_empty() { None } else { Some(branch_true) };
          PreprocessorNodeType::new_if(
            &SourceLoc::from_input(input),
            if_expr.clone(),
            branch_true1.unwrap_or_default(),
            branch_false.unwrap_or_default(),
          )
        } else {
          unreachable!("This code path should not execute")
        }
      },
    )(input)
  }

  /// Parse a `-if(EXPR).\n` and return a temporary node
  pub fn if_directive(input: &str) -> AstParserResult {
    map(
      delimited(
        match_dash_tag("if"),
        delimited(par_open, ws_before(ErlParser::parse_expr), par_close),
        period_newline,
      ),
      // Builds a temporary If node with erl expression in it
      |t| PreprocessorNodeType::new_if_temporary(&SourceLoc::from_input(input), t),
    )(input)
  }

  /// Parse a `-elif(EXPR)` into a temporary AST node
  pub(crate) fn elif_temporary_directive(input: &str) -> AstParserResult {
    map(
      delimited(
        match_dash_tag("elif"),
        delimited(par_open, ws_before(ErlParser::parse_expr), par_close),
        period_newline,
      ),
      |t| PreprocessorNodeType::new_elif_temporary(&SourceLoc::from_input(input), t),
    )(input)
  }

  /// Parse a `-ifdef(MACRO_NAME)`
  pub(crate) fn ifdef_temporary_directive(input: &str) -> AstParserResult {
    map(
      delimited(
        match_dash_tag("ifdef"),
        delimited(par_open, ws_before(Self::macro_ident), par_close),
        period_newline,
      ),
      |t| PreprocessorNodeType::new_ifdef_temporary(&SourceLoc::from_input(input), t),
    )(input)
  }

  /// Parse a `-ifndef(MACRO_NAME)`
  pub fn ifndef_temporary_directive(input: &str) -> AstParserResult {
    map(
      delimited(
        match_dash_tag("ifndef"),
        delimited(par_open, ws_before(Self::macro_ident), par_close),
        period_newline,
      ),
      |t| PreprocessorNodeType::new_ifndef_temporary(&SourceLoc::from_input(input), t),
    )(input)
  }

  /// Parse a `-else.`, return a temporary `Else` node, which will not go into final `PpAst`
  pub fn else_temporary_directive(input: &str) -> AstParserResult {
    map(
      delimited(match_dash_tag("else"), opt(pair(par_open, par_close)), period_newline),
      |_opt| {
        PreprocessorNodeType::construct_with_location(&SourceLoc::from_input(input), _TemporaryElse)
      },
    )(input)
  }

  fn maybe_empty_parens(input: &str) -> StrSliceParserResult {
    recognize(opt(pair(par_open, par_close)))(input)
  }

  /// Parse a `-endif.`, return a temporary `Endif` node, which will not go into final `PpAst`
  pub fn endif_temporary_directive(input: &str) -> AstParserResult {
    map(
      delimited(match_dash_tag("endif"), Self::maybe_empty_parens, period_newline),
      |_opt| {
        PreprocessorNodeType::construct_with_location(
          &SourceLoc::from_input(input),
          _TemporaryEndif,
        )
      },
    )(input)
  }
}

//! Parsers to recognize try-catch and try-of-catch blocks

use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::node::erl_catch_clause::CatchClause;
use crate::syntax_tree::node::erl_exception_pattern::ExceptionPattern;
use crate::syntax_tree::nom_parse::misc::{semicolon, ws_before};
use crate::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use libironclad_error::source_loc::SourceLoc;
use nom::combinator::{cut, map, opt};
use nom::multi::{many1, separated_list1};
use nom::sequence::{preceded, terminated, tuple};
use nom::{bytes::complete::tag, character::complete::char, error::context};

impl ErlParser {
  /// Parse `Class:Error:Stack` triple into `ExceptionPattern`
  pub fn parse_exception_pattern(
    input: &str,
  ) -> nom::IResult<&str, ExceptionPattern, ErlParserError> {
    map(
      tuple((
        Self::parse_matchexpr,
        preceded(ws_before(char(':')), Self::parse_matchexpr),
        opt(preceded(ws_before(char(':')), Self::parse_matchexpr)),
      )),
      |(class_pattern, err_pattern, stack_pattern)| {
        ExceptionPattern::new(class_pattern, err_pattern, stack_pattern)
      },
    )(input)
  }

  /// Parses a repeated catch-clause part after `catch` keyword: `Expr when Expr -> Expr`
  pub fn parse_catch_clause(input: &str) -> nom::IResult<&str, CatchClause, ErlParserError> {
    map(
      tuple((
        // Class:Error:Stacktrace
        Self::parse_exception_pattern,
        // when <Expression>
        opt(preceded(ws_before(tag("when")), Self::parse_guardexpr)),
        // -> Expression
        preceded(ws_before(tag("->")), Self::parse_comma_sep_exprs1::<{ Self::EXPR_STYLE_FULL }>),
      )),
      |(exc_pattern, maybe_when, body)| {
        CatchClause::new(exc_pattern, maybe_when, ErlAst::new_comma_expr(SourceLoc::None, body))
      },
    )(input)
  }

  fn parse_try_catch_inner(input: &str) -> AstParserResult {
    map(
      tuple((
        context(
          "try-catch block trial expression",
          cut(Self::parse_comma_sep_exprs1::<{ Self::EXPR_STYLE_FULL }>),
        ),
        // Optional OF followed by match clauses
        opt(preceded(
          ws_before(tag("of")),
          context("try block: 'of' clauses", cut(many1(ws_before(Self::parse_case_clause)))),
        )),
        // Followed by 1 or more `catch Class:Exception:Stack -> ...` clauses
        preceded(
          ws_before(tag("catch")),
          context(
            "try block: 'catch' clauses",
            cut(separated_list1(semicolon, Self::parse_catch_clause)),
          ),
        ),
      )),
      |(body, of_branches, catch_clauses)| {
        ErlAst::new_try_catch(
          SourceLoc::None,
          ErlAst::new_comma_expr(SourceLoc::None, body),
          of_branches,
          catch_clauses,
        )
      },
    )(input)
  }

  /// Parses a `try-catch` or a `try-of-catch` block
  pub fn parse_try_catch(input: &str) -> AstParserResult {
    let (input, _) = ws_before(tag("try"))(input)?;

    context(
      "try-catch or try-of block",
      cut(terminated(Self::parse_try_catch_inner, ws_before(tag("end")))),
    )(input)
  }
}

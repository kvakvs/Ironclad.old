//! Parsers to recognize try-catch and try-of-catch blocks

use crate::erlang::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use nom::{character::complete::{char}, bytes::complete::{tag}, combinator, error::{context},
          sequence, multi};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_catch_clause::CatchClause;
use crate::erlang::syntax_tree::node::erl_exception_pattern::ExceptionPattern;
use crate::source_loc::SourceLoc;

impl ErlParser {
  /// Parse `Class:Error:Stack` triple into `ExceptionPattern`
  pub fn parse_exception_pattern(input: &str) -> nom::IResult<&str, ExceptionPattern, ErlParserError> {
    combinator::map(
      sequence::tuple((
        Self::parse_expr_no_comma_no_semi,
        Self::ws_before(char(':')),
        Self::parse_expr_no_comma_no_semi,
        combinator::opt(
          sequence::preceded(
            Self::ws_before(char(':')),
            Self::parse_expr_no_comma_no_semi,
          )),
      )),
      |(class_pattern, _colon1, err_pattern, stack_pattern)| {
        ExceptionPattern::new(class_pattern, err_pattern, stack_pattern)
      },
    )(input)
  }

  /// Parses a repeated catch-clause part after `catch` keyword: `Expr when Expr -> Expr`
  pub fn parse_catch_clause(input: &str) -> nom::IResult<&str, CatchClause, ErlParserError> {
    combinator::map(
      sequence::tuple((
        // Class:Error:Stacktrace
        Self::parse_exception_pattern,
        // when <Expression>
        combinator::opt(
          sequence::preceded(
            Self::ws_before(tag("when")),
            Self::parse_expr,
          )
        ),
        // -> Expression
        sequence::preceded(
          Self::ws_before(tag("->")),
          Self::parse_expr,
        )
      )),
      |(exc_pattern, maybe_when, body)| {
        CatchClause::new(exc_pattern, maybe_when, body)
      },
    )(input)
  }

  fn parse_try_catch_inner(input: &str) -> AstParserResult {
    combinator::map(
      sequence::tuple((
        context("try-catch expression", Self::parse_expr),

        // Optional OF followed by match clauses
        combinator::opt(
          sequence::preceded(
            Self::ws_before(tag("of")),
            multi::many1(Self::ws_before(Self::parse_case_clause)),
          )
        ),

        // Followed by 1 or more `catch Class:Exception:Stack -> ...` clauses
        sequence::preceded(
          Self::ws_before(tag("catch")),
          multi::separated_list1(
            Self::ws_before(char(';')),
            Self::parse_catch_clause),
        )
      )),
      |(body, of_branches, catch_clauses)| {
        ErlAst::new_try_catch(SourceLoc::None, body, of_branches, catch_clauses)
      },
    )(input)
  }

  /// Parses a `try-catch` or a `try-of-catch` block
  pub fn parse_try_catch(input: &str) -> AstParserResult {
    sequence::delimited(
      Self::ws_before(tag("try")),
      context("try-catch or try-of-catch block",
              // Self::parse_expr),
              Self::parse_try_catch_inner),
      Self::ws_before(tag("end")),
    )(input)
  }
}
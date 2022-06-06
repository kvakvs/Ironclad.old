//! Parsers to recognize try-catch and try-of-catch blocks

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_catch_clause::CatchClause;
use crate::erl_syntax::node::erl_exception_pattern::ExceptionPattern;
use crate::erl_syntax::parsers::defs::ParserInput;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserResult};
use crate::erl_syntax::parsers::misc::{match_word, semicolon, ws_before};
use crate::erl_syntax::parsers::ErlParser;
use nom::combinator::{cut, map, opt};
use nom::multi::{many1, separated_list1};
use nom::sequence::{preceded, terminated, tuple};
use nom::{bytes::complete::tag, character::complete::char, error::context};

impl ErlParser {
  /// Parse `Class:Error:Stack` triple into `ExceptionPattern`
  pub fn parse_exception_pattern(
    input: ParserInput,
  ) -> nom::IResult<ParserInput, ExceptionPattern, ErlParserError> {
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
  pub fn parse_catch_clause(
    input: ParserInput,
  ) -> nom::IResult<ParserInput, CatchClause, ErlParserError> {
    map(
      tuple((
        // Class:Error:Stacktrace
        Self::parse_exception_pattern,
        // when <Expression>
        opt(preceded(ws_before(tag("when".into())), Self::parse_guardexpr)),
        // -> Expression
        preceded(
          ws_before(tag("->".into())),
          Self::parse_comma_sep_exprs1::<{ Self::EXPR_STYLE_FULL }>,
        ),
      )),
      |(exc_pattern, maybe_when, body)| {
        CatchClause::new(exc_pattern, maybe_when, AstNodeImpl::new_comma_expr(input.loc(), body))
      },
    )(input.clone())
  }

  fn parse_try_catch_inner(input: ParserInput) -> ParserResult<AstNode> {
    map(
      tuple((
        context(
          "try-catch block trial expression",
          cut(Self::parse_comma_sep_exprs1::<{ Self::EXPR_STYLE_FULL }>),
        ),
        // Optional OF followed by match clauses
        opt(preceded(
          match_word("of".into()),
          context("try block: 'of' clauses", cut(many1(ws_before(Self::parse_case_clause)))),
        )),
        // Followed by 1 or more `catch Class:Exception:Stack -> ...` clauses
        preceded(
          match_word("catch".into()),
          context(
            "try block: 'catch' clauses",
            cut(separated_list1(semicolon, Self::parse_catch_clause)),
          ),
        ),
      )),
      |(body, of_branches, catch_clauses)| {
        let loc = input.loc();
        AstNodeImpl::new_try_catch(
          loc.clone(),
          AstNodeImpl::new_comma_expr(loc, body),
          of_branches,
          catch_clauses,
        )
      },
    )(input.clone())
  }

  /// Parses a `try-catch` or a `try-of-catch` block
  pub fn parse_try_catch(input: ParserInput) -> ParserResult<AstNode> {
    let (input, _) = match_word("try".into())(input)?;

    context(
      "try-catch or try-of block",
      cut(terminated(Self::parse_try_catch_inner, match_word("end".into()))),
    )(input)
  }
}

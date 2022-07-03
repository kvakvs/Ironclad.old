//! Helper functions for parsing types

use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  tok_comma, tok_double_colon, tok_keyword_when, tok_par_close, tok_par_open, tok_var,
};
use crate::erl_syntax::parsers::parse_type;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::typing::erl_type::ErlType;
use crate::typing::typevar::Typevar;
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded};

/// Parse part of typevar: `:: type()`, this is to be wrapped in `branch::opt()` by the caller
fn parse_coloncolon_ascription(input: ParserInput) -> ParserResult<ErlType> {
  preceded(tok_double_colon, parse_type::parse_type)(input)
}

/// Parse a capitalized type variable name with an optional `:: type()` part:
/// `A :: type()` or `A`
fn parse_typevar_with_opt_ascription(input: ParserInput) -> ParserResult<Typevar> {
  map(
    pair(
      tok_var,
      opt(context("type ascription for a type variable", parse_coloncolon_ascription)),
    ),
    |(tv_name, maybe_type)| Typevar::new(Some(tv_name), maybe_type),
  )(input)
}

/// Parse a capitalized type variable name with an optional `:: type()` part:
/// `A :: type()` or `A`, or just a type `type()`.
pub fn parse_typevar_or_type(input: ParserInput) -> ParserResult<Typevar> {
  alt((parse_typevar_with_opt_ascription, parse_type_into_typevar))(input)
}

/// Parses a type and wraps it into an anonymous type variable
#[inline]
pub fn parse_type_into_typevar(input: ParserInput) -> ParserResult<Typevar> {
  map(parse_type::parse_type, |t| Typevar::from_erltype(&t))(input)
}

/// Parses a list of comma separated typevars enclosed in (parentheses)
pub(crate) fn parse_parenthesized_arg_spec_list(input: ParserInput) -> ParserResult<Vec<Typevar>> {
  delimited(tok_par_open, list0_types_or_ascribed_typevars, tok_par_close)(input)
}

/// Parse a `when` clause where unspecced typevars can be given type ascriptions, like:
/// `-spec fun(A) -> A when A :: atom().`
pub(crate) fn parse_when_expr_for_type(input: ParserInput) -> ParserResult<Vec<Typevar>> {
  preceded(tok_keyword_when, parse_comma_sep_typeargs1)(input)
}

#[allow(dead_code)]
fn parse_typearg(input: ParserInput) -> ParserResult<Typevar> {
  map(parse_type::parse_type, |t| Typevar::from_erltype(&t))(input)
}

/// Parses a comma separated list of 0 or more type arguments.
/// A parametrized type accepts other types or typevar names
pub fn list0_types_or_ascribed_typevars(input: ParserInput) -> ParserResult<Vec<Typevar>> {
  separated_list0(
    tok_comma,
    context("parsing items of a type arguments list (empty allowed)", parse_typevar_or_type),
  )(input)
}

/// Parses a comma separated list of 1 or more type arguments.
/// A parametrized type accepts other types or typevar names
fn parse_comma_sep_typeargs1(input: ParserInput) -> ParserResult<Vec<Typevar>> {
  separated_list1(
    tok_comma,
    context("parsing items of a type arguments list (non-empty)", parse_typevar_or_type),
  )(input)
}

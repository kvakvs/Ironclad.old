//! Contains parsers for function typespecs and type syntax.

pub mod parse_binary_t;
pub mod parse_container_t;
pub mod parse_fn_t;

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  tok, tok_atom, tok_colon, tok_comma, tok_curly_close, tok_curly_open, tok_hash, tok_integer,
  tok_keyword_when, tok_par_close, tok_par_open, tok_var, tok_vertical_bar,
};
use crate::erl_syntax::parsers::parser_error::ErlParserError;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::{ErlType, ErlTypeImpl};
use crate::typing::typevar::Typevar;
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};

/// Parse part of typevar: `:: type()`, this is to be wrapped in `branch::opt()` by the caller
fn parse_coloncolon_type(input: ParserInput) -> nom::IResult<ParserInput, ErlType, ErlParserError> {
  preceded(
    tok(TokenType::ColonColon),
    context("Type ascription or a type after ::", parse_type),
  )(input)
}

/// Parse a capitalized type variable name with an optional `:: type()` part:
/// `A :: type()` or `A`
fn parse_typevar_with_opt_type(
  input: ParserInput,
) -> nom::IResult<ParserInput, Typevar, ErlParserError> {
  map(pair(parse_typevar_name, opt(parse_coloncolon_type)), |(tv_name, maybe_type)| {
    Typevar::new(Some(tv_name), maybe_type)
  })(input)
}

fn parse_type_as_typevar(input: ParserInput) -> nom::IResult<ParserInput, Typevar, ErlParserError> {
  map(parse_type, |t| Typevar::from_erltype(&t))(input)
}

// /// Parses a list of comma separated typevars (function arg specs)
// fn parse_comma_sep_arg_specs(input: ParserInput) -> nom::IResult<&str, Vec<Typevar>> {
//   separated_list0(
//     ws(char(',')),
//
//     // Comma separated arguments spec can be typevars with optional `::type()`s or just types
//     alt((
//       parse_typevar_with_opt_type,
//       map(parse_type, |t| Typevar::from_erltype(&t)),
//     )),
//   )(input)
// }

/// Parses a list of comma separated typevars enclosed in (parentheses)
pub(crate) fn parse_parenthesized_arg_spec_list(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<Typevar>, ErlParserError> {
  delimited(tok_par_open, comma_sep_typeargs0, tok_par_close)(input)
}

/// Parse a `when` clause where unspecced typevars can be given types, like:
/// `-spec fun(A) -> A when A :: atom().`
pub(crate) fn parse_when_expr_for_type(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<Typevar>, ErlParserError> {
  preceded(tok_keyword_when, parse_comma_sep_typeargs1)(input)
}

/// Parse only capitalized type variable name
#[inline]
pub(crate) fn parse_typevar_name(input: ParserInput) -> ParserResult<'_, String> {
  tok_var(input)
}

fn alt_typevar_or_type(input: ParserInput) -> nom::IResult<ParserInput, Typevar, ErlParserError> {
  alt((
    parse_typevar_with_opt_type,
    map(parse_type, |t| Typevar::from_erltype(&t)),
    // map(parse_typevar, |tvname| Typevar::new(Some(tvname), None)),
  ))(input)
}

#[allow(dead_code)]
fn parse_typearg(input: ParserInput) -> nom::IResult<ParserInput, Typevar, ErlParserError> {
  map(parse_type, |t| Typevar::from_erltype(&t))(input)
}

/// Parses a comma separated list of 0 or more type arguments.
/// A parametrized type accepts other types or typevar names
fn comma_sep_typeargs0(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<Typevar>, ErlParserError> {
  separated_list0(tok_comma, context("parsing items of a typeargs0_list", alt_typevar_or_type))(
    input,
  )
}

/// Parses a comma separated list of 1 or more type arguments.
/// A parametrized type accepts other types or typevar names
fn parse_comma_sep_typeargs1(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<Typevar>, ErlParserError> {
  separated_list1(tok_comma, context("parsing items of a typeargs1_list", alt_typevar_or_type))(
    input,
  )
}

/// Optional `module:` before typename in `module:type()`.
fn parse_type_modulename_colon(
  input: ParserInput,
) -> nom::IResult<ParserInput, String, ErlParserError> {
  terminated(tok_atom, tok_colon)(input)
}

/// Parse a user defined type with `name()` and 0 or more typevar args.
/// Optional with module name `module:name()`.
fn user_defined_type(input: ParserInput) -> ParserResult<ErlType> {
  map(
    tuple((
      opt(parse_type_modulename_colon),
      tok_atom,
      delimited(
        tok_par_open,
        context("type arguments for a user-defined type", comma_sep_typeargs0),
        tok_par_close,
      ),
    )),
    |(maybe_module, type_name, elements)| {
      ErlTypeImpl::from_name(maybe_module, type_name, &elements)
    },
  )(input)
}

/// Parse a record type reference with `#tagname{}`, does not define a record, refers to an existing
fn record_ref(input: ParserInput) -> nom::IResult<ParserInput, ErlType, ErlParserError> {
  map(
    preceded(tok_hash, pair(tok_atom, pair(tok_curly_open, tok_curly_close))),
    |(tag, (_, _))| ErlTypeImpl::new_record_ref(tag),
  )(input)
}

/// Parse an integer and produce a literal integer type
fn int_literal_type(input: ParserInput) -> nom::IResult<ParserInput, ErlType, ErlParserError> {
  map(tok_integer, |i: ErlInteger| {
    // TODO: Can a parsed integer parse with an error?
    ErlTypeImpl::new_singleton(&Literal::Integer(i).into())
  })(input)
}

/// Parse an integer range
fn int_range_type(input: ParserInput) -> ParserResult<ErlType> {
  map(
    separated_pair(tok_integer, tok(TokenType::PeriodPeriod), tok_integer),
    |(a, b)| {
      // TODO: Can a parsed integer parse with an error?
      ErlTypeImpl::new_range(a, b)
    },
  )(input)
}

/// Parse an atom, and produce a literal atom type
fn atom_literal_type(input: ParserInput) -> ParserResult<ErlType> {
  map(tok_atom, |a_str| ErlTypeImpl::new_singleton(&Literal::Atom(a_str).into()))(input)
}

/// Parse any simple Erlang type without union. To parse unions use `parse_type`.
fn parse_nonunion_type(input: ParserInput) -> nom::IResult<ParserInput, ErlType, ErlParserError> {
  alt((
    parse_binary_t::binary_type,
    int_range_type,
    parse_container_t::type_of_nonempty_list,
    parse_container_t::type_of_list,
    parse_container_t::type_of_tuple,
    parse_container_t::type_of_map,
    record_ref,
    user_defined_type,
    int_literal_type,
    atom_literal_type,
  ))(input)
}

/// Parse any Erlang type, simple types like `atom()` with some `(args)` possibly, but could also be
/// a structured type like union of multiple types `atom()|number()`, a list or a tuple of types, etc
pub(crate) fn parse_type(input: ParserInput) -> nom::IResult<ParserInput, ErlType, ErlParserError> {
  map(separated_list1(tok_vertical_bar, parse_nonunion_type), |types| {
    ErlTypeImpl::new_union(&types)
  })(input)
}

/// Wraps parsed type into a type-AST-node
pub fn parse_type_node(input: ParserInput) -> ParserResult<AstNode> {
  map(parse_type, |t| {
    AstNodeImpl::construct_with_location(SourceLoc::new(&input), AstNodeType::Type { ty: t })
  })(input.clone())
}

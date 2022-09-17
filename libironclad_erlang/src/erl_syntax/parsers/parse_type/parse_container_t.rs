//! Parsing types for containers: list, tuple and map.

use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc_tok::*;
use crate::erl_syntax::parsers::parse_type::parse_t_util::{
  list0_types_or_ascribed_typevars, parse_typevar_or_type,
};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::typing::erl_type::map_type::MapMemberType;
use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::{ErlType, TypeImpl};
use nom::branch::alt;
use nom::combinator::map;
use nom::error::context;
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, separated_pair, terminated};

/// Parse a list of types, returns a temporary list-type
pub fn type_of_list(input: ParserInput) -> ParserResult<ErlType> {
  map(
    delimited(
      tok_square_open,
      context("type arguments for a list() type", list0_types_or_ascribed_typevars),
      tok_square_close,
    ),
    |vec_of_t| TypeImpl::new_unnamed(TypeKind::list_of_types(vec_of_t)),
  )(input)
}

/// Parse a list of type and ellipsis, creating a nonempty list-type
pub fn type_of_nonempty_list(input: ParserInput) -> ParserResult<ErlType> {
  map(
    delimited(
      tok_square_open,
      context(
        "type arguments for a nonempty_list() type",
        terminated(parse_typevar_or_type, pair(tok_comma, tok_ellipsis)),
      ),
      tok_square_close,
    ),
    |list_el_type| TypeImpl::new_unnamed(TypeKind::list_of(list_el_type, true)),
  )(input)
}

/// Parse a tuple of types, returns a temporary tuple-type
pub fn type_of_tuple(input: ParserInput) -> ParserResult<ErlType> {
  map(
    delimited(
      tok_curly_open,
      context("a tuple() type", list0_types_or_ascribed_typevars),
      tok_curly_close,
    ),
    |vec_of_t| TypeImpl::new_unnamed(TypeKind::new_tuple_move(vec_of_t)),
  )(input)
}

fn map_member_type(input: ParserInput) -> ParserResult<MapMemberType> {
  map(
    separated_pair(parse_typevar_or_type, alt((tok_assign, tok_right_darr)), parse_typevar_or_type),
    |(key, value)| MapMemberType { key, value },
  )(input)
}

/// Parses a comma separated list of map field types
fn comma_sep_map_members0(input: ParserInput) -> ParserResult<Vec<MapMemberType>> {
  separated_list0(tok_comma, context("parsing member types of a map type", map_member_type))(input)
}

/// Parse a map of types, returns a map-type
pub fn type_of_map(input: ParserInput) -> ParserResult<ErlType> {
  map(
    delimited(
      pair(tok_hash, tok_curly_open),
      context("a map() type", comma_sep_map_members0),
      tok_curly_close,
    ),
    |vec_of_members| TypeImpl::new_unnamed(TypeKind::new_map(vec_of_members)),
  )(input)
}

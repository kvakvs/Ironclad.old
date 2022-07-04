//! Contains parsers for function typespecs and type syntax.

pub mod parse_binary_t;
pub mod parse_container_t;
pub mod parse_fn_t;
pub mod parse_t_util;

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::lang_construct::LangConstruct;
use crate::erl_syntax::parsers::misc;
use crate::erl_syntax::parsers::misc::{
  tok_atom, tok_colon, tok_curly_close, tok_curly_open, tok_double_period, tok_hash, tok_integer,
  tok_par_close, tok_par_open, tok_vertical_bar,
};
use crate::erl_syntax::parsers::parse_type::parse_binary_t::binary_type;
use crate::erl_syntax::parsers::parse_type::parse_container_t::{
  type_of_list, type_of_map, type_of_nonempty_list, type_of_tuple,
};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::{ErlType, ErlTypeImpl};
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::multi::separated_list1;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::Parser;

/// Parse a user defined type with `name()` and 0 or more typevar args.
/// Optional with module name `module:name()`.
fn parse_user_type(input: ParserInput) -> ParserResult<ErlType> {
  map(
    tuple((
      opt(terminated(tok_atom, tok_colon)),
      tok_atom,
      delimited(
        tok_par_open,
        context(
          "type arguments for a user-defined type",
          parse_t_util::list0_types_or_ascribed_typevars,
        ),
        tok_par_close,
      ),
    )),
    |(maybe_module, type_name, elements)| {
      ErlTypeImpl::from_name(maybe_module, type_name, &elements)
    },
  )(input)
}

/// Parse a record type reference with `#tagname{}`, does not define a record, refers to an existing
fn record_ref(input: ParserInput) -> ParserResult<ErlType> {
  map(
    preceded(tok_hash, terminated(tok_atom, pair(tok_curly_open, tok_curly_close))),
    ErlTypeImpl::new_record_ref,
  )(input)
}

/// Parse an integer and produce a literal integer type
fn int_literal_type(input: ParserInput) -> ParserResult<ErlType> {
  map(tok_integer, |i: ErlInteger| {
    // TODO: Can a parsed integer parse with an error?
    ErlTypeImpl::new_singleton(&Literal::Integer(i).into())
  })(input)
}

/// Parse an integer range
fn int_range_type(input: ParserInput) -> ParserResult<ErlType> {
  map(separated_pair(tok_integer, tok_double_period, tok_integer), |(a, b)| {
    // TODO: Can a parsed integer parse with an error?
    ErlTypeImpl::new_range(a, b)
  })(input)
}

/// Parse an atom, and produce a literal atom type
fn atom_literal_type(input: ParserInput) -> ParserResult<ErlType> {
  map(tok_atom, |a_str| ErlTypeImpl::new_singleton(&Literal::Atom(a_str).into()))(input)
}

/// Parse any simple Erlang type without union. To parse unions use `parse_type`.
fn parse_nonunion_type<'a>(input: ParserInput<'a>) -> ParserResult<ErlType> {
  let alt_failed = |i: ParserInput<'a>| -> ParserResult<ErlType> {
    misc::alt_failed(
      i,
      "a non-union type",
      &[
        LangConstruct::UserType,
        LangConstruct::IntegerRangeType,
        LangConstruct::ListType,
        LangConstruct::TupleType,
        LangConstruct::MapType,
        LangConstruct::RecordRefType,
        LangConstruct::IntegerLiteralType,
        LangConstruct::AtomLiteralType,
      ],
    )
  };
  context(
    "non-union type parser",
    alt((
      context("user defined type", parse_user_type),
      binary_type,
      context("integer range", int_range_type),
      context("non-empty list type", type_of_nonempty_list),
      context("list type", type_of_list),
      context("tuple type", type_of_tuple),
      context("map type", type_of_map),
      context("record reference type", record_ref),
      context("integer literal type", int_literal_type),
      context("atom literal type", atom_literal_type),
    ))
    .or(alt_failed),
  )(input.clone())
}

/// Parse any Erlang type, simple types like `atom()` with some `(args)` possibly, but could also be
/// a structured type like union of multiple types `atom()|number()`, a list or a tuple of types, etc
pub(crate) fn parse_type(input: ParserInput) -> ParserResult<ErlType> {
  map(separated_list1(tok_vertical_bar, parse_nonunion_type), |types| {
    ErlTypeImpl::new_union(&types)
  })(input)
}

/// Wraps parsed type into a type-AST-node, testing only
pub fn parse_type_as_ast_node(input: ParserInput) -> ParserResult<AstNode> {
  map(parse_type, |t| {
    AstNodeImpl::construct_with_location(SourceLoc::new(&input), AstNodeType::Type { ty: t })
  })(input.clone())
}

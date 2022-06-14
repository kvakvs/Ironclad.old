//! Parse binary expressions and binary builders.

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_binary_element::{
  BinaryElement, TypeSpecifier, ValueEndianness, ValueSignedness, ValueType, ValueWidth,
};
use crate::erl_syntax::parsers::defs::ParserInput;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{tok, tok_atom_of, tok_integer, tok_var};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_lit::parse_erl_literal;
use crate::erl_syntax::preprocessor::parsers::parse_macro::macro_invocation_as_ast_node;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
use crate::typing::erl_integer::ErlInteger;
use nom::branch::alt;
use nom::combinator::{cut, map, opt};
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{bytes::complete::tag, character::complete::char, error::context};
use std::ops::Deref;

/// Parse a literal value, variable, or an expression in parentheses.
fn bin_element_value(input: ParserInput) -> ParserResult<AstNode> {
  alt((
    // Expect an expression if a macro is expanded here
    macro_invocation_as_ast_node,
    map(tok_var, |v| AstNodeImpl::new_var(SourceLoc::new(input), &v)),
    parse_erl_literal,
    delimited(tok(TokenType::ParOpen), parse_expr, tok(TokenType::ParClose)),
  ))(input.clone())
}

/// Parse a `:Number`, `:Variable` or `:(Expr)` for bit width
fn bin_element_width(input: ParserInput) -> ParserResult<ValueWidth> {
  map(bin_element_value, |v| {
    if let AstNodeType::Lit { value: lit_val, .. } = &v.content {
      if let Literal::Integer(i) = lit_val.deref() {
        assert!(i.is_non_negative());
        ValueWidth::Literal(i.as_usize().unwrap()) // TODO: Error if value too big
      } else {
        panic!("For bit width only positive integers are accepted")
      }
    } else {
      ValueWidth::Expr(v)
    }
  })(input)
}

fn bin_element_typespec_type(input: ParserInput) -> ParserResult<TypeSpecifier> {
  alt((
    map(tok_atom_of("integer"), |_| TypeSpecifier::Type(ValueType::Integer)),
    map(tok_atom_of("float"), |_| TypeSpecifier::Type(ValueType::Float)),
    map(alt((tok_atom_of("bytes"), tok_atom_of("binary"))), |_| {
      TypeSpecifier::Type(ValueType::Bytes)
    }),
    map(alt((tok_atom_of("bitstring"), tok_atom_of("bits"))), |_| {
      TypeSpecifier::Type(ValueType::Bitstring)
    }),
    map(tok_atom_of("utf8"), |_| TypeSpecifier::Type(ValueType::Utf8)),
    map(tok_atom_of("utf16"), |_| TypeSpecifier::Type(ValueType::Utf16)),
    map(tok_atom_of("utf32"), |_| TypeSpecifier::Type(ValueType::Utf32)),
  ))(input)
}

fn bin_element_typespec_signedness(input: ParserInput) -> ParserResult<TypeSpecifier> {
  alt((
    map(tag("signed".into()), |_| TypeSpecifier::Signedness(ValueSignedness::Signed)),
    map(tag("unsigned".into()), |_| TypeSpecifier::Signedness(ValueSignedness::Unsigned)),
  ))(input)
}

fn bin_element_typespec_endianness(input: ParserInput) -> ParserResult<TypeSpecifier> {
  alt((
    map(tag("big".into()), |_| TypeSpecifier::Endianness(ValueEndianness::Big)),
    map(tag("little".into()), |_| TypeSpecifier::Endianness(ValueEndianness::Little)),
    map(tag("native".into()), |_| TypeSpecifier::Endianness(ValueEndianness::Native)),
  ))(input)
}

fn bin_element_typespec_unit(input: ParserInput) -> ParserResult<TypeSpecifier> {
  map(
    preceded(terminated(tok_atom_of("unit"), tok(TokenType::Colon)), tok_integer),
    |i: ErlInteger| TypeSpecifier::Unit(i.as_usize().unwrap_or_default()),
  )(input)
}

fn bin_element_type_spec(input: ParserInput) -> ParserResult<TypeSpecifier> {
  alt((
    bin_element_typespec_type,
    bin_element_typespec_signedness,
    bin_element_typespec_endianness,
    bin_element_typespec_unit,
  ))(input)
}

/// Parse a `-` separated list of typespecs
fn bin_type_specs(input: ParserInput) -> ParserResult<Vec<TypeSpecifier>> {
  preceded(
    tok(TokenType::Div),
    separated_list1(tok(TokenType::Minus), bin_element_type_spec),
  )(input)
}

/// Parse one comma-separated element of a binary: number, variable, an expression,
/// and followed by a `:bit-width` and `/type-specifier`
fn bin_element(input: ParserInput) -> ParserResult<BinaryElement> {
  map(
    tuple((
      bin_element_value,
      opt(preceded(tok(TokenType::Colon), bin_element_width)),
      opt(bin_type_specs),
    )),
    |(value, bit_width, type_specs)| {
      BinaryElement::new(
        SourceLoc::None,
        value,
        bit_width.unwrap_or(ValueWidth::Default),
        type_specs.unwrap_or_default(),
      )
    },
  )(input)
}

/// Parse a binary or binary builder expression
pub(crate) fn parse_binary(input: ParserInput) -> ParserResult<AstNode> {
  preceded(
    tok(TokenType::DoubleAngleOpen),
    context(
      "binary expression",
      cut(terminated(
        map(
          separated_list0(tok(TokenType::Comma), context("binary expression element", bin_element)),
          |bin_exprs| AstNodeImpl::new_binary_expr(SourceLoc::new(input), bin_exprs),
        ),
        tok(TokenType::DoubleAngleClose),
      )),
    ),
  )(input.clone())
}

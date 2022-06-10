//! Parse binary expressions and binary builders.

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_binary_element::{
  BinaryElement, TypeSpecifier, ValueEndianness, ValueSignedness, ValueType, ValueWidth,
};
use crate::erl_syntax::parsers::defs::ParserInput;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  comma_tag, par_close_tag, par_open_tag, parse_int, parse_varname, ws_before,
};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_lit::parse_erl_literal;
use crate::erl_syntax::preprocessor::parsers::parse_macro::macro_invocation_as_ast_node;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
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
    map(parse_varname, |v| AstNodeImpl::new_var(input.loc(), &v)),
    parse_erl_literal,
    delimited(par_open_tag, parse_expr, par_close_tag),
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
    map(tag("integer".into()), |_| TypeSpecifier::Type(ValueType::Integer)),
    map(tag("float".into()), |_| TypeSpecifier::Type(ValueType::Float)),
    map(alt((tag("bytes".into()), tag("binary".into()))), |_| {
      TypeSpecifier::Type(ValueType::Bytes)
    }),
    map(alt((tag("bitstring".into()), tag("bits".into()))), |_| {
      TypeSpecifier::Type(ValueType::Bitstring)
    }),
    map(tag("utf8".into()), |_| TypeSpecifier::Type(ValueType::Utf8)),
    map(tag("utf16".into()), |_| TypeSpecifier::Type(ValueType::Utf16)),
    map(tag("utf32".into()), |_| TypeSpecifier::Type(ValueType::Utf32)),
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
  map(preceded(tag("unit:".into()), parse_int), |erl_int| {
    TypeSpecifier::Unit(erl_int.as_usize().unwrap_or_default())
  })(input)
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
    ws_before(char('/')), // TODO: Whitespace allowed before?
    separated_list1(
      ws_before(char('-')), // TODO: Whitespace allowed before?
      bin_element_type_spec,
    ),
  )(input)
}

/// Parse one comma-separated element of a binary: number, variable, an expression,
/// and followed by a `:bit-width` and `/type-specifier`
fn bin_element(input: ParserInput) -> ParserResult<BinaryElement> {
  map(
    tuple((
      bin_element_value,
      opt(preceded(ws_before(char(':')), bin_element_width)),
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
    ws_before(tag("<<".into())),
    context(
      "binary expression",
      cut(terminated(
        map(
          separated_list0(comma_tag, context("binary expression element", ws_before(bin_element))),
          |bin_exprs| AstNodeImpl::new_binary_expr(input.loc(), bin_exprs),
        ),
        ws_before(tag(">>".into())),
      )),
    ),
  )(input.clone())
}

//! Parse binary expressions and binary builders.

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_binary_element::{
  BinaryElement, TypeSpecifier, ValueEndianness, ValueSignedness, ValueType, ValueWidth,
};
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::lang_construct::LangConstruct;
use crate::erl_syntax::parsers::misc;
use crate::erl_syntax::parsers::misc::{
  tok_atom_of, tok_colon, tok_comma, tok_double_angle_close, tok_double_angle_open,
  tok_forward_slash, tok_integer, tok_minus, tok_par_close, tok_par_open, tok_var,
};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_lit::parse_erl_literal;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
use crate::typing::erl_integer::ErlInteger;
use nom::branch::alt;
use nom::combinator::{cut, map, opt, value};
use nom::error::context;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::Parser;
use std::ops::Deref;

/// Parse a literal value, variable, or an expression in parentheses.
fn bin_element_value<'a>(input: ParserInput<'a>) -> ParserResult<AstNode> {
  let alt_failed = |i: ParserInput<'a>| -> ParserResult<AstNode> {
    misc::alt_failed(
      i,
      "binary element",
      &[
        LangConstruct::Variable,
        LangConstruct::Literal,
        LangConstruct::ParenthesizedExpression,
      ],
    )
  };
  context(
    "element of a binary value",
    alt((
      map(tok_var, |v| AstNodeImpl::new_var(SourceLoc::new(&input), &v)),
      parse_erl_literal,
      delimited(tok_par_open, parse_expr, tok_par_close),
    ))
    .or(alt_failed),
  )(input.clone())
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

fn bin_element_typespec_type<'a>(input: ParserInput<'a>) -> ParserResult<TypeSpecifier> {
  let alt_failed = |i: ParserInput<'a>| -> ParserResult<TypeSpecifier> {
    misc::alt_failed(
      i,
      "binary element type spec",
      &[
        LangConstruct::AtomOf("integer"),
        LangConstruct::AtomOf("float"),
        LangConstruct::AtomOf("bytes"),
        LangConstruct::AtomOf("binary"),
        LangConstruct::AtomOf("bitstring"),
        LangConstruct::AtomOf("bits"),
        LangConstruct::AtomOf("utf8"),
        LangConstruct::AtomOf("utf16"),
        LangConstruct::AtomOf("utf32"),
      ],
    )
  };

  context(
    "spec for a binary element",
    alt((
      value(TypeSpecifier::Type(ValueType::Integer), tok_atom_of("integer")),
      value(TypeSpecifier::Type(ValueType::Float), tok_atom_of("float")),
      value(TypeSpecifier::Type(ValueType::Bytes), tok_atom_of("bytes")),
      value(TypeSpecifier::Type(ValueType::Bytes), tok_atom_of("binary")),
      value(TypeSpecifier::Type(ValueType::Bitstring), tok_atom_of("bitstring")),
      value(TypeSpecifier::Type(ValueType::Bitstring), tok_atom_of("bits")),
      value(TypeSpecifier::Type(ValueType::Utf8), tok_atom_of("utf8")),
      value(TypeSpecifier::Type(ValueType::Utf16), tok_atom_of("utf16")),
      value(TypeSpecifier::Type(ValueType::Utf32), tok_atom_of("utf32")),
    ))
    .or(alt_failed),
  )(input)
}

fn bin_element_typespec_signedness<'a>(input: ParserInput<'a>) -> ParserResult<TypeSpecifier> {
  let alt_failed = |i: ParserInput<'a>| -> ParserResult<TypeSpecifier> {
    misc::alt_failed(
      i,
      "binary element signedness",
      &[
        LangConstruct::AtomOf("signed"),
        LangConstruct::AtomOf("unsigned"),
      ],
    )
  };
  context(
    "binary element sign choice",
    alt((
      map(tok_atom_of("signed"), |_| TypeSpecifier::Signedness(ValueSignedness::Signed)),
      map(tok_atom_of("unsigned"), |_| {
        TypeSpecifier::Signedness(ValueSignedness::Unsigned)
      }),
    ))
    .or(alt_failed),
  )(input)
}

fn bin_element_typespec_endianness<'a>(input: ParserInput<'a>) -> ParserResult<TypeSpecifier> {
  let alt_failed = |i: ParserInput<'a>| -> ParserResult<TypeSpecifier> {
    misc::alt_failed(
      i,
      "binary element byte order",
      &[
        LangConstruct::AtomOf("big"),
        LangConstruct::AtomOf("little"),
        LangConstruct::AtomOf("native"),
      ],
    )
  };
  context(
    "binary element endianness choice",
    alt((
      map(tok_atom_of("big"), |_| TypeSpecifier::Endianness(ValueEndianness::Big)),
      map(tok_atom_of("little"), |_| TypeSpecifier::Endianness(ValueEndianness::Little)),
      map(tok_atom_of("native"), |_| TypeSpecifier::Endianness(ValueEndianness::Native)),
    ))
    .or(alt_failed),
  )(input)
}

fn bin_element_typespec_unit(input: ParserInput) -> ParserResult<TypeSpecifier> {
  map(
    preceded(terminated(tok_atom_of("unit"), tok_colon), tok_integer),
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
  preceded(tok_forward_slash, separated_list1(tok_minus, bin_element_type_spec))(input)
}

/// Parse one comma-separated element of a binary: number, variable, an expression,
/// and followed by a `:bit-width` and `/type-specifier`
fn bin_element(input: ParserInput) -> ParserResult<BinaryElement> {
  map(
    tuple((
      bin_element_value,
      opt(preceded(tok_colon, bin_element_width)),
      opt(bin_type_specs),
    )),
    |(value, bit_width, type_specs)| {
      BinaryElement::new(
        SourceLoc::None,
        value,
        bit_width.unwrap_or(ValueWidth::DefaultWidth),
        type_specs.unwrap_or_default(),
      )
    },
  )(input)
}

/// Parse a binary or binary builder expression
pub(crate) fn parse_binary(input: ParserInput) -> ParserResult<AstNode> {
  preceded(
    tok_double_angle_open,
    context(
      "binary expression",
      cut(terminated(
        map(
          separated_list0(tok_comma, context("binary expression element", bin_element)),
          |bin_exprs| AstNodeImpl::new_binary_expr(SourceLoc::new(&input), bin_exprs),
        ),
        tok_double_angle_close,
      )),
    ),
  )(input.clone())
}

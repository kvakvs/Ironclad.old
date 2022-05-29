//! Parse ironclad_exe expressions and ironclad_exe builders.

use crate::erl_syntax::erl_ast::{ErlAst, ErlAstType};
use crate::erl_syntax::node::erl_binary_element::{
  BinaryElement, TypeSpecifier, ValueEndianness, ValueSignedness, ValueType, ValueWidth,
};
use crate::erl_syntax::parsers::misc::{
  comma, par_close, par_open, parse_int, parse_varname, ws_before,
};
use crate::erl_syntax::parsers::{AstParserResult, ErlParser, ParserResult};
use crate::literal::Literal;
use libironclad_error::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::{cut, map, opt};
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{bytes::complete::tag, character::complete::char, error::context};
use std::ops::Deref;
use std::str::FromStr;

/// Wraps functions for parsing binaries and ironclad_exe builders
pub struct BinaryParser {}

impl BinaryParser {
  /// Parse a literal value, variable, or an expression in parentheses.
  fn parse_value(input: &str) -> AstParserResult {
    alt((
      map(parse_varname, |v| ErlAst::new_var(&SourceLoc::from_input(input), &v)),
      ErlParser::parse_literal,
      delimited(par_open, ErlParser::parse_expr, par_close),
    ))(input)
  }

  /// Parse a `:Number`, `:Variable` or `:(Expr)` for bit width
  fn parse_width(input: &str) -> ParserResult<ValueWidth> {
    map(Self::parse_value, |v| {
      if let ErlAstType::Lit { value: lit_val, .. } = &v.content {
        // TODO: Big Integer
        if let Literal::Integer(inner_val) = lit_val.deref() {
          assert!(*inner_val > 0);
          ValueWidth::Literal(inner_val.abs() as usize)
        } else {
          panic!("For bit width only positive integers are accepted")
        }
      } else {
        ValueWidth::Expr(v)
      }
    })(input)
  }

  fn parse_typespec_type(input: &str) -> ParserResult<TypeSpecifier> {
    alt((
      map(tag("integer"), |_| TypeSpecifier::Type(ValueType::Integer)),
      map(tag("float"), |_| TypeSpecifier::Type(ValueType::Float)),
      map(alt((tag("bytes"), tag("ironclad_exe"))), |_| {
        TypeSpecifier::Type(ValueType::Bytes)
      }),
      map(alt((tag("bitstring"), tag("bits"))), |_| {
        TypeSpecifier::Type(ValueType::Bitstring)
      }),
      map(tag("utf8"), |_| TypeSpecifier::Type(ValueType::Utf8)),
      map(tag("utf16"), |_| TypeSpecifier::Type(ValueType::Utf16)),
      map(tag("utf32"), |_| TypeSpecifier::Type(ValueType::Utf32)),
    ))(input)
  }

  fn parse_typespec_signedness(input: &str) -> ParserResult<TypeSpecifier> {
    alt((
      map(tag("signed"), |_| TypeSpecifier::Signedness(ValueSignedness::Signed)),
      map(tag("unsigned"), |_| TypeSpecifier::Signedness(ValueSignedness::Unsigned)),
    ))(input)
  }

  fn parse_typespec_endianness(input: &str) -> ParserResult<TypeSpecifier> {
    alt((
      map(tag("big"), |_| TypeSpecifier::Endianness(ValueEndianness::Big)),
      map(tag("little"), |_| TypeSpecifier::Endianness(ValueEndianness::Little)),
      map(tag("native"), |_| TypeSpecifier::Endianness(ValueEndianness::Native)),
    ))(input)
  }

  fn parse_typespec_unit(input: &str) -> ParserResult<TypeSpecifier> {
    map(preceded(tag("unit:"), parse_int), |i_str| {
      TypeSpecifier::Unit(usize::from_str(i_str).unwrap())
    })(input)
  }

  fn parse_a_type_spec(input: &str) -> ParserResult<TypeSpecifier> {
    alt((
      Self::parse_typespec_type,
      Self::parse_typespec_signedness,
      Self::parse_typespec_endianness,
      Self::parse_typespec_unit,
    ))(input)
  }

  /// Parse a `-` separated list of typespecs
  fn parse_type_specs(input: &str) -> ParserResult<Vec<TypeSpecifier>> {
    preceded(
      ws_before(char('/')), // TODO: Whitespace allowed before?
      separated_list1(
        ws_before(char('-')), // TODO: Whitespace allowed before?
        Self::parse_a_type_spec,
      ),
    )(input)
  }

  /// Parse one comma-separated element of a ironclad_exe: number, variable, an expression,
  /// and followed by a `:bit-width` and `/type-specifier`
  fn parse_bin_element(input: &str) -> ParserResult<BinaryElement> {
    map(
      tuple((
        Self::parse_value,
        opt(preceded(ws_before(char(':')), Self::parse_width)),
        opt(Self::parse_type_specs),
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

  /// Parse a ironclad_exe or ironclad_exe builder expression
  pub fn parse(input: &str) -> AstParserResult {
    let (input, _) = ws_before(tag("<<"))(input)?;

    context(
      "ironclad_exe expression",
      cut(terminated(
        map(
          separated_list1(
            comma,
            context("ironclad_exe expression element", cut(ws_before(Self::parse_bin_element))),
          ),
          |bin_exprs| ErlAst::new_binary_expr(&SourceLoc::from_input(input), bin_exprs),
        ),
        ws_before(tag(">>")),
      )),
    )(input)
  }
}

//! Binary types parsing

use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::lang_construct::LangConstruct;
use crate::erl_syntax::parsers::misc;
use crate::erl_syntax::parsers::misc::tok_integer;
use crate::erl_syntax::parsers::misc_tok::*;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::typing::erl_type::binary_type::{BinaryTypeHeadElement, BinaryTypeTailElement};
use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::{ErlType, TypeImpl};
use nom::branch::alt;
use nom::combinator::{cut, map, opt};
use nom::error::context;
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::Parser;

/// Binary type optional starting element: `_ : INTEGER`
pub fn binary_type_head_element(input: ParserInput) -> ParserResult<BinaryTypeHeadElement> {
  map(preceded(pair(tok_underscore, tok_colon), tok_integer), |width| {
    BinaryTypeHeadElement(width.as_usize().unwrap())
  })(input)
}

/// Binary type optional tail element: `_ : _ * INTEGER`
pub fn binary_type_tail_element(input: ParserInput) -> ParserResult<BinaryTypeTailElement> {
  map(
    preceded(tuple((tok_underscore, tok_colon, tok_underscore, tok_asterisk)), tok_integer),
    |repeat| BinaryTypeTailElement(repeat.as_usize().unwrap()),
  )(input)
}

// #[inline]
// fn binary_type_head(input: ParserInput) -> ParserResult<ErlType> {
//   map(
//     context("binary type with head element only", binary_type_head_element),
//     |head| ErlTypeImpl::new_binary(Some(head), None),
//   )(input)
// }

#[inline]
fn binary_type_tail(input: ParserInput) -> ParserResult<ErlType> {
  map(
    context("binary type with tail element only", binary_type_tail_element),
    |tail| TypeImpl::new_unnamed(TypeKind::new_binary(None, Some(tail))),
  )(input)
}

#[inline]
fn binary_type_head_tail(input: ParserInput) -> ParserResult<ErlType> {
  map(
    context(
      "binary type with head and tail elements",
      pair(binary_type_head_element, opt(preceded(tok_comma, binary_type_tail_element))),
    ),
    |(head, tail)| TypeImpl::new_unnamed(TypeKind::new_binary(Some(head), tail)),
  )(input)
}

/// Parse a binary type `<< _ : 8, _:_ * 8 >>`. Shortcuts:
///
/// *   `binary()`             | `<<_:_*8>>`
/// *   `nonempty_binary()`    | `<<_:8, _:_*8>>`
/// *   `bitstring()`          | `<<_:_*1>>`
/// *   `nonempty_bitstring()` | `<<_:1, _:_*1>>`
///
/// The general form of bit strings is `<<_:M, _:_*N>>`, where `M` and `N` must evaluate to
/// positive integers. It denotes a bit string that is `M + (k*N)` bits long (that is, a bit
/// string that starts with `M` bits and continues with `k` segments of `N` bits each,
/// where `k` is also a positive integer). The notations `<<_:_*N>>`, `<<_:M>>`, and `<<>>` are
/// convenient shorthands for the cases that `M` or `N`, or both, are zero.
pub fn binary_type(input: ParserInput) -> ParserResult<ErlType> {
  delimited(
    tok_double_angle_open,
    context(
      "binary type",
      cut(alt((binary_type_tail, binary_type_head_tail)))
        .or(|i| misc::alt_failed(i, "binary type", &[LangConstruct::BinaryType])),
    ),
    tok_double_angle_close,
  )(input)
}

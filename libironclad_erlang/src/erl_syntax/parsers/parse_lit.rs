//! Parse literal values as the occur in source code

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::node_impl::AstNodeType::Lit;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{tok_atom, tok_string, ws_before};
use crate::erl_syntax::parsers::misc_tok::*;
use crate::erl_syntax::parsers::parse_lit_numbers;
use crate::erl_syntax::parsers::parser_error::ErlParserError;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::token_stream::token::Token;
use crate::erl_syntax::parsers::token_stream::token_kind::TokenKind;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::{consumed, map};
use nom::error::context;
use nom::multi::many1;
use nom::sequence::pair;
use nom::Slice;
use std::sync::Arc;

fn parse_string_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(many1(tok_string), |tokens| {
    let s = tokens
      .iter()
      .map(|t| t.as_str())
      .collect::<Vec<_>>()
      .join("");
    AstNodeImpl::construct_with_location(
      SourceLoc::new(&input),
      Lit { value: Literal::String(Arc::new(s)).into() },
    )
  })(input.clone())
}

fn parse_atom_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(tok_atom, |s| {
    AstNodeImpl::construct_with_location(
      SourceLoc::new(&input),
      Lit { value: Literal::Atom(s).into() },
    )
  })(input.clone())
}

fn parse_nil(input: ParserInput) -> ParserResult<AstNode> {
  let make_nil = |(consumed_input, ((), ())): (ParserInput, ((), ()))| -> AstNode {
    AstNodeImpl::new_nil(SourceLoc::new(&consumed_input))
  };
  map(consumed(pair(tok_square_open, tok_square_close)), make_nil)(input.clone())
}

fn parse_empty_binary(input: ParserInput) -> ParserResult<AstNode> {
  let make_empty = |(consumed_input, ((), ())): (ParserInput, ((), ()))| -> AstNode {
    AstNodeImpl::new_empty_binary(SourceLoc::new(&consumed_input))
  };
  map(consumed(pair(tok_double_angle_open, tok_double_angle_close)), make_empty)(input.clone())
}

fn tok_character_1(input: ParserInput) -> ParserResult<AstNode> {
  match input.tokens.iter().next() {
    Some(Token { kind: TokenKind::Character(c), .. }) => {
      let node = AstNodeImpl::new_lit_character(SourceLoc::new(&input), *c);
      Ok((input.slice(1..), node))
    }
    Some(Token {
      kind: TokenKind::EscapedCharacter { value, in_source },
      ..
    }) => {
      let node = AstNodeImpl::new_lit_escaped_character(SourceLoc::new(&input), *value, *in_source);
      Ok((input.slice(1..), node))
    }
    _other => Err(nom::Err::Error(ErlParserError::character_literal_expected(input))),
  }
}

/// Recognizes a character token
#[inline]
fn parse_character(input: ParserInput) -> ParserResult<AstNode> {
  ws_before(tok_character_1)(input)
}

/// Read a literal value from input string.
/// This does not parse `-` as a part of literal, and instead `-` becomes an unary expression.
/// See `parse_erl_literal_with_sign` for consuming `-`.
pub(crate) fn parse_erl_literal(input: ParserInput) -> ParserResult<AstNode> {
  context(
    "literal",
    ws_before(alt((
      parse_nil,
      parse_empty_binary,
      parse_lit_numbers::parse_float_to_ast,
      parse_lit_numbers::parse_int_to_ast,
      parse_atom_to_ast,
      parse_string_to_ast,
      parse_character,
    ))),
  )(input)
}

// /// Read a literal value from input string. This parses `-` as a part of literal.
// /// See `parse_erl_literal` for not consuming the `-`.
// pub(crate) fn parse_erl_literal_with_sign(input: ParserInput) -> ParserResult<AstNode> {
//   context(
//     "literal",
//     ws_before(alt((
//       parse_nil,
//       parse_empty_binary,
//       parse_lit_numbers::parse_float_to_ast_with_sign,
//       parse_lit_numbers::parse_int_to_ast_with_sign,
//       parse_atom_to_ast,
//       parse_string_to_ast,
//       parse_character,
//     ))),
//   )(input)
// }

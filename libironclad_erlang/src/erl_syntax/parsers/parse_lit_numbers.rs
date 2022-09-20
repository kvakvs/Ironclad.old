//! Parse tools for numbers consumption

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::node_impl::AstNodeType::Lit;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{tok_float, tok_integer};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
use crate::typing::erl_integer::ErlInteger;
use nom::combinator::map;

/// Consume a float (or an integer?), sign is not recognised.
/// Use `parse_float_to_ast_with_sign` to consume the sign too.
pub fn parse_float_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(tok_float, |f: f64| {
    let lit_node = Lit { value: Literal::Float(f).into() };
    AstNodeImpl::construct_with_location(SourceLoc::new(&input), lit_node)
  })(input.clone())
}

/// Consume an integer, sign is not recognised. Use `parse_int_to_ast_with_sign` to consume the sign too.
pub fn parse_int_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(tok_integer, |i: ErlInteger| {
    let lit_node = Lit {
      // TODO: Can parsed integer create a parse error?
      value: Literal::Integer(i).into(),
    };
    AstNodeImpl::construct_with_location(SourceLoc::new(&input), lit_node)
  })(input.clone())
}

// /// Produced by parse_sign when parsing signed numbers
// enum Sign {
//   Positive,
//   Negative,
// }

// /// Parses a plus or minus
// fn parse_sign(input: ParserInput) -> ParserResult<Sign> {
//   map(consumed(alt((tok_plus, tok_minus))), |(pi, _): (ParserInput, _)| {
//     if pi.tokens[0].kind == TokenKind::Plus {
//       Sign::Positive
//     } else if pi.tokens[0].kind == TokenKind::Minus {
//       Sign::Negative
//     } else {
//       panic!("Sign parser encountered {:?} which is neither + or -", pi)
//     }
//   })(input)
// }

// /// Consumes a preceding sign, and then consumes a float. Use this when a constant is expected and
// /// an unary expr with `-` is not wanted.
// pub fn parse_float_to_ast_with_sign(input: ParserInput) -> ParserResult<AstNode> {
//   map(pair(opt(parse_sign), tok_float), |(maybe_sign, f): (Option<Sign>, f64)| {
//     let lit_node = Lit { value: Literal::Float(f).into() };
//     AstNodeImpl::construct_with_location(SourceLoc::new(&input), lit_node)
//   })(input.clone())
// }

// /// Consumes a preceding sign, and then consumes an integer. Use this when a constant is expected and
// /// an unary expr with `-` is not wanted.
// pub fn parse_int_to_ast_with_sign(input: ParserInput) -> ParserResult<AstNode> {}

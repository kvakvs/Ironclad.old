//! Parse list structures in expressions

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc_tok::{
  tok_comma, tok_double_vertical_bar, tok_square_close, tok_square_open, tok_vertical_bar,
};
use crate::erl_syntax::parsers::parse_expr::{
  parse_comprehension_exprs_and_generators, parse_expr,
};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::source_loc::SourceLoc;
use nom::combinator::{consumed, cut, map, opt};
use nom::error::context;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, separated_pair};

/// Parse a list of expressions with optional tail
pub fn parse_list_builder(input: ParserInput) -> ParserResult<AstNode> {
  let build_fn = |(consumed_input, (head, (mut elements, maybe_tail))): (
    ParserInput,
    (AstNode, (Vec<AstNode>, Option<AstNode>)),
  )|
   -> AstNode {
    elements.insert(0, head);
    AstNodeImpl::new_list(SourceLoc::new(&consumed_input), elements, maybe_tail)
  };

  // A square bracket delimited sequence, where
  // First element is expr
  // Followed by comma+expr, or bar+expr
  // "[" <EXPR> [ ("," <EXPR>) | ("|" <EXPR>) ] "]"
  map(
    consumed(delimited(
      tok_square_open,
      pair(
        parse_expr,
        pair(
          many0(preceded(tok_comma, parse_expr)),
          opt(preceded(tok_vertical_bar, parse_expr)),
        ),
      ),
      tok_square_close,
    )),
    build_fn,
  )(input.clone())
}

// pub fn parse_list_builder(input: ParserInput) -> ParserResult<AstNode> {
//   let build_fn = |(consumed_input, (elements, maybe_tail)): (
//     ParserInput,
//     (Vec<AstNode>, Option<AstNode>),
//   )|
//    -> AstNode {
//     AstNodeImpl::new_list(SourceLoc::new(&consumed_input), elements, maybe_tail)
//   };
//
//   map(
//     consumed(delimited(
//       tok_square_open,
//       pair(
//         parse_comma_sep_exprs0,
//         opt(preceded(tok_vertical_bar, parse_expr)),
//       ),
//       tok_square_close,
//     )),
//     build_fn,
//   )(input.clone())
// }

fn parse_list_comprehension_1(input: ParserInput) -> ParserResult<AstNode> {
  let mk_list_comp =
    |(consumed_input, (expr, generators)): (ParserInput, (AstNode, Vec<AstNode>))| -> AstNode {
      AstNodeImpl::new_list_comprehension(SourceLoc::new(&consumed_input), expr, generators)
    };

  map(
    consumed(separated_pair(
      context("list comprehension output expression", parse_expr),
      tok_double_vertical_bar,
      context("list comprehension generators", cut(parse_comprehension_exprs_and_generators)),
    )),
    mk_list_comp,
  )(input.clone())
}

/// Public for testing. Parses a list comprehension syntax `[ OUTPUT | GENERATORS ]`
pub fn parse_list_comprehension(input: ParserInput) -> ParserResult<AstNode> {
  context(
    "list comprehension",
    delimited(tok_square_open, parse_list_comprehension_1, tok_square_close),
  )(input)
}

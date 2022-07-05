//! Parse expressions and guard expressions (with added ;, operators)

pub mod parse_expr_list;
pub mod parse_expr_map;
pub mod parse_expr_prec;
pub mod parse_expr_record;

use crate::erl_syntax::erl_ast::expr_style::ExprStyle;
use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::node_impl::AstNodeType::Var;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_var::ErlVar;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{tok_atom, tok_integer, tok_var};
use crate::erl_syntax::parsers::misc_tok::*;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::{consumed, cut, map, opt};
use nom::error::context;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated};

fn parse_var(input: ParserInput) -> ParserResult<AstNode> {
  let mk_var = |(consumed_input, n): (ParserInput, String)| -> AstNode {
    AstNodeImpl::construct_with_location(SourceLoc::new(&consumed_input), Var(ErlVar::new(&n)))
  };

  map(consumed(tok_var), mk_var)(input)
}

/// Parses a list of comma separated expressions in (parentheses)
pub fn parse_parenthesized_list_of_exprs(input: ParserInput) -> ParserResult<Vec<AstNode>> {
  delimited(
    tok_par_open,
    context("function application arguments", cut(parse_comma_sep_exprs0)),
    tok_par_close,
  )(input)
}

fn parse_binary_comprehension_1(input: ParserInput) -> ParserResult<AstNode> {
  map(
    separated_pair(
      context("binary comprehension output expression", parse_expr),
      tok_double_vertical_bar,
      context("binary comprehension generators", cut(parse_comprehension_exprs_and_generators)),
    ),
    |(expr, generators): (AstNode, Vec<AstNode>)| -> AstNode {
      AstNodeImpl::new_binary_comprehension(SourceLoc::new(&input), expr, generators)
    },
  )(input.clone())
}

/// Parses a binary comprehension syntax `<< OUTPUT || VAR1 <- GENERATOR1, COND1 ... >>`
fn parse_binary_comprehension(input: ParserInput) -> ParserResult<AstNode> {
  context(
    "binary comprehension",
    delimited(tok_double_angle_open, parse_binary_comprehension_1, tok_double_angle_close),
  )(input)
}

/// Parse a sequence of curly braced expressions `"{" EXPR1 "," EXPR2 "," ... "}"`
fn parse_tuple_builder(input: ParserInput) -> ParserResult<AstNode> {
  map(delimited(tok_curly_open, parse_comma_sep_exprs0, tok_curly_close), |elements| {
    AstNodeImpl::new_tuple(SourceLoc::new(&input), elements)
  })(input.clone())
}

/// Parses comma separated sequence of expressions
pub fn parse_comma_sep_exprs0(input: ParserInput) -> ParserResult<Vec<AstNode>> {
  separated_list0(
    tok_comma,
    // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
    parse_expr,
  )(input)
}

/// Parses comma separated sequence of expressions, at least one or more
pub fn parse_comma_sep_exprs1(input: ParserInput) -> ParserResult<Vec<AstNode>> {
  separated_list1(tok_comma, parse_expr)(input)
}

/// Parse an expression in parentheses `"(" EXPR ")"`
pub fn parenthesized_expr(input: ParserInput) -> ParserResult<AstNode> {
  delimited(tok_par_open, parse_expr, tok_par_close)(input)
}

/// Parses a `begin-end` grouping
pub(crate) fn parse_begin_end(input: ParserInput) -> ParserResult<AstNode> {
  let map_fn =
    |exprs: Vec<AstNode>| -> AstNode { AstNodeImpl::new_begin_end(SourceLoc::new(&input), exprs) };
  map(
    delimited(
      keyword_begin,
      context(
        "contents of a begin-end expression",
        cut(separated_list1(tok_comma, parse_expr)),
      ),
      keyword_end,
    ),
    map_fn,
  )(input.clone())
}

/// Parses `fun [module :] function / arity`
fn parse_fn_reference(input: ParserInput) -> ParserResult<AstNode> {
  map(
    consumed(preceded(
      keyword_fun,
      pair(
        opt(terminated(tok_atom, tok_colon)),
        separated_pair(tok_atom, tok_forward_slash, tok_integer),
      ),
    )),
    |(consumed_input, (module, (function, arity)))| {
      AstNodeImpl::new_fn_ref(
        SourceLoc::new(&consumed_input),
        module,
        function,
        arity.as_usize().unwrap(),
      )
    },
  )(input)
}

/// Parse an expression OR a function application which is essentially `EXPR ( EXPRS... )`.
#[inline]
/// Express the intent of parsing any expression.
pub fn parse_expr(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_prec::parse_expr_lowest_precedence(ExprStyle::Full, input)
}

#[inline]
/// Express the intent of parsing a match expression.
/// This does not do checking of what's parsed, and the result might contain pieces disallowed in
/// match expressions. Run `AstNodeImpl::verify_expr_style` after the parse.
pub fn parse_matchexpr(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_prec::parse_expr_lowest_precedence(ExprStyle::MatchExpr, input)
}

#[inline]
/// Express the intent of parsing a guard expression.
/// This does not do checking of what's parsed, and the result might contain pieces disallowed in
/// guards. Run `AstNodeImpl::verify_expr_style` after the parse.
pub fn parse_guardexpr(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_prec::parse_expr_lowest_precedence(ExprStyle::Guard, input)
}

#[inline]
/// Express the intent of parsing a const expression.
/// This does not do checking of what's parsed, and the result might contain non-const pieces.
/// Run `AstNodeImpl::verify_expr_style` after the parse.
pub fn parse_constant_expr(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_prec::parse_expr_lowest_precedence(ExprStyle::Const, input)
}

/// Parses a `Expr <- Expr` generator for a list or binary comprehension
fn parse_comprehension_generator(input: ParserInput) -> ParserResult<AstNode> {
  let make_comp_gen = |(consumed_input, (a, b)): (ParserInput, (AstNode, AstNode))| -> AstNode {
    AstNodeImpl::new_list_comprehension_generator(SourceLoc::new(&consumed_input), a, b)
  };
  map(consumed(separated_pair(parse_expr, tok_left_arrow, parse_expr)), make_comp_gen)(
    input.clone(),
  )
}

/// Parses mix of generators and conditions for a list or binary comprehension
pub fn parse_comprehension_exprs_and_generators(input: ParserInput) -> ParserResult<Vec<AstNode>> {
  separated_list1(
    tok_comma,
    // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
    alt((parse_comprehension_generator, parse_expr)),
  )(input)
}

//! Parse expressions and guard expressions (with added ;, operators)

use ::function_name::named;

use crate::erl_syntax::erl_ast::expr_style::ExprStyle;
use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::node_impl::AstNodeType::Var;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_callable_target::CallableTarget;
use crate::erl_syntax::node::erl_map::MapBuilderMember;
use crate::erl_syntax::node::erl_record::RecordBuilderMember;
use crate::erl_syntax::node::erl_unop::ErlUnaryOperatorExpr;
use crate::erl_syntax::node::erl_var::ErlVar;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  tok, tok_atom, tok_colon, tok_comma, tok_curly_close, tok_curly_open, tok_double_angle_close,
  tok_double_angle_open, tok_double_vertical_bar, tok_hash, tok_keyword_begin, tok_keyword_end,
  tok_left_arrow, tok_par_close, tok_par_open, tok_period, tok_square_close, tok_square_open,
  tok_var, tok_vertical_bar,
};
use crate::erl_syntax::parsers::parse_binary::parse_binary;
use crate::erl_syntax::parsers::parse_case::parse_case_expr;
use crate::erl_syntax::parsers::parse_expr_op::{
  binop_add, binop_and, binop_andalso, binop_band, binop_bang, binop_bor, binop_bsl, binop_bsr,
  binop_bxor, binop_comma, binop_equals, binop_floatdiv, binop_greater, binop_greater_eq,
  binop_hard_equals, binop_hard_not_equals, binop_intdiv, binop_less, binop_less_eq,
  binop_list_append, binop_list_subtract, binop_match, binop_multiply, binop_not_equals, binop_or,
  binop_orelse, binop_rem, binop_semicolon, binop_subtract, binop_xor, unop_bnot, unop_catch,
  unop_negative, unop_not, unop_positive,
};
use crate::erl_syntax::parsers::parse_fn::parse_lambda;
use crate::erl_syntax::parsers::parse_if_stmt::parse_if_statement;
use crate::erl_syntax::parsers::parse_lit::parse_erl_literal;
use crate::erl_syntax::parsers::parse_try_catch::parse_try_catch;
use crate::erl_syntax::parsers::parser_error::ErlParserError;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::error::ic_error::IcResult;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::{consumed, cut, map, opt};
use nom::error::context;
use nom::multi::{many0, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};

fn parse_var(input: ParserInput) -> ParserResult<AstNode> {
  let mk_var = |(consumed_input, n): (ParserInput, String)| -> AstNode {
    AstNodeImpl::construct_with_location(SourceLoc::new(&consumed_input), Var(ErlVar::new(&n)))
  };

  map(consumed(tok_var), mk_var)(input)
}

/// Parses a list of comma separated expressions in (parentheses)
pub fn parse_parenthesized_list_of_exprs(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<AstNode>, ErlParserError> {
  delimited(
    tok_par_open,
    context("function application arguments", cut(parse_comma_sep_exprs0)),
    tok_par_close,
  )(input)
}

fn parse_list_builder(input: ParserInput) -> ParserResult<AstNode> {
  let build_fn = |(consumed_input, (elements, maybe_tail)): (
    ParserInput,
    (Vec<AstNode>, Option<AstNode>),
  )|
   -> AstNode {
    AstNodeImpl::new_list(SourceLoc::new(&consumed_input), elements, maybe_tail)
  };

  map(
    consumed(delimited(
      tok_square_open,
      pair(parse_comma_sep_exprs0, opt(preceded(tok_vertical_bar, parse_expr))),
      tok_square_close,
    )),
    build_fn,
  )(input.clone())
}

/// Parses a `Expr <- Expr` generator
pub fn parse_list_comprehension_generator(input: ParserInput) -> ParserResult<AstNode> {
  let make_comp_gen = |(consumed_input, (a, b)): (ParserInput, (AstNode, AstNode))| -> AstNode {
    AstNodeImpl::new_list_comprehension_generator(SourceLoc::new(&input), a, b)
  };
  map(consumed(separated_pair(parse_expr, tok_left_arrow, parse_expr)), make_comp_gen)(
    input.clone(),
  )
}

/// Parses mix of generators and conditions for a list comprehension
pub fn parse_list_comprehension_exprs_and_generators(
  input: ParserInput,
) -> ParserResult<Vec<AstNode>> {
  separated_list1(
    tok_comma,
    // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
    alt((parse_list_comprehension_generator, parse_expr)),
  )(input)
}

fn parse_list_comprehension_1(input: ParserInput) -> ParserResult<AstNode> {
  let mk_list_comp =
    |(consumed_input, (expr, generators)): (ParserInput, (AstNode, Vec<AstNode>))| -> AstNode {
      AstNodeImpl::new_list_comprehension(SourceLoc::new(&consumed_input), expr, generators)
    };

  map(
    consumed(separated_pair(
      context("list comprehension output expression", parse_expr),
      tok_double_vertical_bar,
      context(
        "list comprehension generators",
        cut(parse_list_comprehension_exprs_and_generators),
      ),
    )),
    mk_list_comp,
  )(input.clone())
}

fn parse_binary_comprehension_1(input: ParserInput) -> ParserResult<AstNode> {
  map(
    separated_pair(
      context("binary comprehension output expression", parse_expr),
      tok_double_vertical_bar,
      context(
        "binary comprehension generators",
        cut(parse_list_comprehension_exprs_and_generators),
      ),
    ),
    |(expr, generators): (AstNode, Vec<AstNode>)| -> AstNode {
      AstNodeImpl::new_binary_comprehension(SourceLoc::new(&input), expr, generators)
    },
  )(input.clone())
}

/// Public for testing. Parses a list comprehension syntax `[ OUTPUT | GENERATORS ]`
pub fn parse_list_comprehension(input: ParserInput) -> ParserResult<AstNode> {
  context(
    "list comprehension",
    delimited(tok_square_open, parse_list_comprehension_1, tok_square_close),
  )(input)
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

/// Parse one member of a map builder `keyExpr "=>" valueExpr`
fn map_builder_member(input: ParserInput) -> ParserResult<MapBuilderMember> {
  map(
    separated_pair(parse_expr, tok(TokenType::RightDoubleArr), parse_expr),
    |(key, expr)| MapBuilderMember { key, expr },
  )(input)
}

/// Parse one member of a record builder `'field' = EXPR`
fn record_builder_member(input: ParserInput) -> ParserResult<RecordBuilderMember> {
  map(
    separated_pair(tok_atom, tok(TokenType::EqualSymbol), parse_expr),
    |(field, expr)| RecordBuilderMember { field, expr },
  )(input)
}

/// Parse a map builder expression, which uses `=>` to assign the values.
/// Contrary to a map matcher, which would use `:=`.
fn parse_map_builder(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      pair(tok_hash, tok_curly_open),
      separated_list0(tok_comma, map_builder_member),
      tok_curly_close,
    ),
    |members| AstNodeImpl::new_map_builder(SourceLoc::new(&input), members),
  )(input.clone())
}

/// Parse a record builder expression
fn parse_record_builder(input: ParserInput) -> ParserResult<(String, Vec<RecordBuilderMember>)> {
  terminated(
    pair(
      delimited(tok_hash, tok_atom, tok_curly_open),
      separated_list0(tok_comma, record_builder_member),
    ),
    tok_curly_close,
  )(input.clone())
}

/// Parse a record builder expression without a prefix expression just `# RECORDTAG { FIELDS }`
fn parse_record_builder_no_base(input: ParserInput) -> ParserResult<AstNode> {
  map(parse_record_builder, |(tag, record_fields)| {
    AstNodeImpl::new_record_builder(SourceLoc::new(&input), None, tag, record_fields)
  })(input.clone())
}

/// Parse a record field access expression. Matches `# RECORDTAG "." FIELDTAG`
/// Returns (record: String, field: String)
fn parse_record_field_access(input: ParserInput) -> ParserResult<(String, String)> {
  pair(delimited(tok_hash, tok_atom, tok_period), tok_atom)(input.clone())
}

/// Parses comma separated sequence of expressions
pub fn parse_comma_sep_exprs0(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<AstNode>, ErlParserError> {
  separated_list0(
    tok_comma,
    // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
    parse_expr,
  )(input)
}

/// Parses comma separated sequence of expressions, at least one or more
pub fn parse_comma_sep_exprs1(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<AstNode>, ErlParserError> {
  separated_list1(tok_comma, parse_expr)(input)
}

fn parenthesized_expr(input: ParserInput) -> ParserResult<AstNode> {
  delimited(tok_par_open, parse_expr, tok_par_close)(input)
}

/// Parses a `begin-end` grouping
pub(crate) fn parse_begin_end(input: ParserInput) -> ParserResult<AstNode> {
  let map_fn =
    |exprs: Vec<AstNode>| -> AstNode { AstNodeImpl::new_begin_end(SourceLoc::new(&input), exprs) };
  map(
    delimited(
      tok_keyword_begin,
      context("contents of a begin...end block", cut(separated_list1(tok_comma, parse_expr))),
      tok_keyword_end,
    ),
    map_fn,
  )(input.clone())
}

/// Priority 0: (Parenthesized expressions), numbers, variables, negation (unary ops)
fn parse_expr_prec_primary(input: ParserInput) -> ParserResult<AstNode> {
  context(
    "[hidden] parse expression (highest precedence)",
    alt((
      alt((
        parse_lambda,
        parse_begin_end,
        parse_try_catch,
        parse_if_statement,
        parse_case_expr,
        parenthesized_expr,
        parse_list_builder,
        parse_tuple_builder,
        parse_map_builder,
      )),
      alt((
        parse_record_builder_no_base,
        parse_var,
        parse_erl_literal,
        parse_list_comprehension,
        parse_binary_comprehension,
        parse_binary,
      )),
    )),
  )(input)
}

// TODO: Precedence 1: : (colon operator, for bit fields and module access?)
// TODO: module:function notation and maybe tuple notation?
/// Parse expr followed by a parentheses with 0 or more args, to become a function call
fn parse_expr_prec01(input: ParserInput) -> ParserResult<AstNode> {
  map(
    tuple((
      parse_expr_prec_primary,
      // An optional second expression after a ':', MUST be followed by parentheses with args
      opt(
        // A pair: optional second expr for function name, and mandatory args
        pair(
          opt(preceded(tok_colon, parse_expr_prec_primary)),
          parse_parenthesized_list_of_exprs,
        ),
      ),
    )),
    |(expr1, maybe_expr2_args)| {
      if let Some((maybe_expr2, args)) = maybe_expr2_args {
        match maybe_expr2 {
          Some(expr2) => {
            // TODO: merge match clause 2 and 3 as new_mfa_expr should be doing job of both?
            let target = CallableTarget::new_mfa_expr(Some(expr1), expr2, args.len());
            AstNodeImpl::new_application(SourceLoc::new(&input), target, args)
          }
          None => {
            let target = CallableTarget::new_expr(expr1);
            AstNodeImpl::new_application(SourceLoc::new(&input), target, args)
          }
        }
      } else {
        expr1 // no extra args after the expression
      }
    },
  )(input.clone())
}

// TODO: Precedence 2: # (record access operator)
#[inline]
fn parse_expr_prec02(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_prec01(input)
}

/// Precedence 3: Unary + - bnot not
fn parse_expr_prec03(input: ParserInput) -> ParserResult<AstNode> {
  map(
    consumed(pair(
      alt((unop_negative, unop_positive, unop_bnot, unop_not)),
      parse_expr_prec02,
    )),
    mk_unary,
  )(input.clone())
  .or_else(|_err| parse_expr_prec02(input.clone()))
}

/// Precedence 4: / * div rem band and, left associative
fn parse_expr_prec04(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    consumed(pair(
      parse_expr_prec03,
      many0(pair(
        alt((binop_floatdiv, binop_multiply, binop_intdiv, binop_rem, binop_band, binop_and)),
        parse_expr_prec03,
      )),
    )),
    mk_binop_left_assoc,
  )(input)
}

/// Precedence 5: + - bor bxor bsl bsr or xor, left associative
fn parse_expr_prec05(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    consumed(tuple((
      parse_expr_prec04,
      many0(pair(
        alt((
          binop_add,
          binop_bor,
          binop_bsl,
          binop_bsr,
          binop_bxor,
          binop_or,
          binop_subtract,
          binop_xor,
        )),
        parse_expr_prec04,
      )),
    ))),
    mk_binop_left_assoc,
  )(input)
}

/// Precedence 6: ++ --, right associative
fn parse_expr_prec06(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    consumed(pair(
      parse_expr_prec05,
      many0(pair(alt((binop_list_append, binop_list_subtract)), parse_expr_prec05)),
    )),
    mk_binop_right_assoc,
  )(input)
}

/// Precedence 7: == /= =< < >= > =:= =/=
fn parse_expr_prec07(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    consumed(pair(
      parse_expr_prec06,
      many0(pair(
        alt((
          binop_hard_equals,
          binop_hard_not_equals,
          binop_not_equals,
          binop_equals,
          binop_less_eq,
          binop_less,
          binop_greater_eq,
          binop_greater,
        )),
        parse_expr_prec06,
      )),
    )),
    mk_binop_left_assoc,
  )(input)
}

/// Precedence 8: andalso
fn parse_expr_prec08(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more ANDALSO operators and higher prec exprs
    consumed(pair(parse_expr_prec07, many0(pair(binop_andalso, parse_expr_prec07)))),
    mk_binop_left_assoc,
  )(input)
}

#[inline]
/// Call this with result of nom's `consumed()` combinator
fn mk_binop_left_assoc(
  (consumed_input, (left, tail)): (ParserInput, (AstNode, Vec<(ErlBinaryOp, AstNode)>)),
) -> AstNode {
  ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::new(&consumed_input), left, &tail)
}

/// Precedence 9: orelse
fn parse_expr_prec09(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more ORELSE operators and higher prec exprs
    consumed(pair(parse_expr_prec08, many0(pair(binop_orelse, parse_expr_prec08)))),
    mk_binop_left_assoc,
  )(input)
}

#[inline]
fn mk_binop_right_assoc(
  (consumed_input, (left, tail)): (ParserInput, (AstNode, Vec<(ErlBinaryOp, AstNode)>)),
) -> AstNode {
  ErlBinaryOperatorExpr::new_right_assoc(SourceLoc::new(&consumed_input), left, &tail)
}

/// Precedence 10: assignment/match = operator, and send operator "!", right associative
fn parse_expr_prec10(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
    consumed(pair(
      parse_expr_prec09,
      many0(pair(alt((binop_match, binop_bang)), parse_expr_prec09)),
    )),
    mk_binop_right_assoc,
  )(input)
}

#[inline]
/// Wrap values passed here with `consumed()` nom combinator
fn mk_unary((consumed_input, (catch_op, expr)): (ParserInput, (ErlUnaryOp, AstNode))) -> AstNode {
  ErlUnaryOperatorExpr::new_ast(SourceLoc::new(&consumed_input), catch_op, expr)
}

/// Precedence 11: Catch operator, then continue to higher precedences
/// This is also entry point to parse expression when you don't want to recognize comma and semicolon
fn parse_expr_prec11(input: ParserInput) -> ParserResult<AstNode> {
  // Try parse (catch Expr) otherwise try next precedence level
  map(consumed(pair(unop_catch, parse_expr_prec10)), mk_unary)(input.clone())
    .or_else(|_err| parse_expr_prec10(input.clone()))
}

/// Lowest precedence 13, where we handle comma and semicolon as binary ops.
/// Note that semicolon is not valid for regular code only allowed in guards.
#[named]
fn parse_expr_prec13(style: ExprStyle, input: ParserInput) -> ParserResult<AstNode> {
  // Only guard style expressions allow comma/semicolon as binary ops
  if style == ExprStyle::Guard {
    // Guard-style expressions allow both comma and semicolons
    map(
      // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
      pair(
        parse_expr_prec11,
        many0(pair(alt((binop_semicolon, binop_comma)), parse_expr_prec11)),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::None, left, &tail),
    )(input)
  } else {
    parse_expr_prec11(input)
  }
}

fn parse_expr_lowest_precedence(style: ExprStyle, input: ParserInput) -> ParserResult<AstNode> {
  map(
    context(
      "expression",
      tuple((
        |i| parse_expr_prec13(style, i),
        opt(parse_parenthesized_list_of_exprs),
        opt(parse_record_field_access),
        opt(parse_record_builder),
      )),
    ),
    |(expr, maybe_args, maybe_field_access, maybe_record_builder): (
      AstNode,
      Option<Vec<AstNode>>,
      Option<(String, String)>,
      Option<(String, Vec<RecordBuilderMember>)>,
    )|
     -> AstNode {
      if let Some(args) = maybe_args {
        let target = CallableTarget::new_expr(expr);
        AstNodeImpl::new_application(SourceLoc::new(&input), target, args)
      } else if let Some((tag, field)) = maybe_field_access {
        // TODO: This goes to precedence02
        AstNodeImpl::new_record_field(SourceLoc::new(&input), expr, tag, field)
      } else if let Some((tag, record_fields)) = maybe_record_builder {
        AstNodeImpl::new_record_builder(SourceLoc::new(&input), Some(expr), tag, record_fields)
      } else {
        expr
      }
    },
  )(input.clone())
}

/// Parse an expression OR a function application which is essentially `EXPR ( EXPRS... )`.
#[inline]
/// Express the intent of parsing any expression.
pub fn parse_expr(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_lowest_precedence(ExprStyle::Full, input)
}

#[inline]
/// Express the intent of parsing a match expression.
/// This does not do checking of what's parsed, and the result might contain pieces disallowed in
/// match expressions. Run `AstNodeImpl::verify_expr_style` after the parse.
pub fn parse_matchexpr(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_lowest_precedence(ExprStyle::MatchExpr, input)
}

#[inline]
/// Express the intent of parsing a guard expression.
/// This does not do checking of what's parsed, and the result might contain pieces disallowed in
/// guards. Run `AstNodeImpl::verify_expr_style` after the parse.
pub fn parse_guardexpr(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_lowest_precedence(ExprStyle::Guard, input)
}

#[inline]
/// Express the intent of parsing a const expression.
/// This does not do checking of what's parsed, and the result might contain non-const pieces.
/// Run `AstNodeImpl::verify_expr_style` after the parse.
pub fn parse_constant_expr(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_lowest_precedence(ExprStyle::Const, input)
}

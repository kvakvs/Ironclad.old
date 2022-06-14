//! Parse helpers for `-define`/`-undef` preprocessor

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{
  parenthesis_period_newline, period_newline, tok, tok_atom, tok_atom_of,
};
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::parsers::preprocessor::{comma_sep_macro_idents, macro_ident};
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::character::complete::anychar;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::multi::many_till;
use nom::sequence::{delimited, preceded, tuple};

/// Parses inner part of a `-define(IDENT)` variant
fn define_no_args_no_body(input: ParserInput) -> ParserResult<AstNode> {
  map(macro_ident, |name: String| {
    input.parser_scope.define(&name, &[], "");
    AstNodeImpl::new_empty(format!("-define({}).", &name))
  })(input.clone())
}

/// Parses inner part of a `-define(IDENT(ARGS), BODY).` or without args `-define(IDENT, BODY).`
/// will consume end delimiter `").\n"`
fn define_with_args_body_and_terminator(input: ParserInput) -> ParserResult<AstNode> {
  map(
    tuple((
      // Macro name
      macro_ident,
      // Optional (ARG1, ARG2, ...) with trailing comma
      opt(delimited(
        tok(TokenType::ParOpen),
        comma_sep_macro_idents,
        tok(TokenType::ParClose),
      )),
      tok(TokenType::Comma),
      // Followed by a body
      many_till(anychar, parenthesis_period_newline),
    )),
    |(ident, args, _comma, (body, _term))| {
      let body_str = body.into_iter().collect::<String>();
      let args1 = args.unwrap_or_default();
      input.parser_scope.define(&ident, &args1, &body_str);
      println!("New scope {:?}", &input);
      AstNodeImpl::new_empty(format!("-define({}/{}, ...)", &ident, args1.len()))
    },
  )(input.clone())
  // let result = tuple((
  //   // Macro name
  //   macro_ident,
  //   // Optional (ARG1, ARG2, ...) with trailing comma
  //   opt(delimited(par_open_tag, comma_sep_macro_idents, par_close_tag)),
  //   comma_tag,
  //   // Followed by a body
  //   ws_before_mut(many_till(anychar, parenthesis_dot_newline)),
  // ))(input.clone());

  // if result.is_ok() {
  //   let (input_out, (name, args, _comma, (body, _terminator))) = result.unwrap();
  //   let body_str = body.into_iter().collect::<String>();
  //   input_out.preprocessor_define(&name, &args.unwrap_or_default(), &body_str);
  //
  //   println!("Updating scope: Define {} = {}", &name, &body_str);
  //
  //   println!("New scope {:?}", &input_out);
  //   Ok((input_out, AstNodeImpl::new_empty()))
  // } else {
  //   Err(result.unwrap_err())
  // }
}

/// Parse a `-define(NAME)` or `-define(NAME, VALUE)` or `-define(NAME(ARGS,...), VALUE)`
pub(crate) fn define_directive(input: ParserInput) -> ParserResult<AstNode> {
  preceded(
    tok_atom_of("define"),
    alt((
      context(
        "-define directive with no args and no body",
        delimited(tok(TokenType::ParOpen), define_no_args_no_body, parenthesis_period_newline),
      ),
      // `define_with_args_body_and_terminator` will consume end delimiter
      context(
        "-define directive with optional args and body",
        preceded(tok(TokenType::ParOpen), define_with_args_body_and_terminator),
      ),
    )),
  )(input)
}

/// Parse a `-undef(IDENT)`
pub(crate) fn undef_directive(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      tok_atom_of("undef"),
      delimited(tok(TokenType::ParOpen), macro_ident, tok(TokenType::ParClose)),
      period_newline,
    ),
    |ident: String| PreprocessorNodeType::new_undef(SourceLoc::new(input), ident),
  )(input.clone())
}

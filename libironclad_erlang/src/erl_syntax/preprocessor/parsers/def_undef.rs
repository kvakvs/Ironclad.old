//! Parse helpers for `-define`/`-undef` preprocessor

use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  dash_atom, parenthesis_period_newline, period_eol, tok, tok_comma, tok_par_close, tok_par_open,
};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::if_ifdef::tok_macro_ident;
use crate::erl_syntax::preprocessor::parsers::preprocessor::comma_sep_macro_idents;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::misc::any_token;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::multi::many_till;
use nom::sequence::{delimited, preceded, tuple};

/// Parses inner part of a `-define(IDENT)` variant
fn define_no_args_no_body(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(tok_macro_ident, |name: String| {
    PreprocessorNodeImpl::new_define(SourceLoc::new(&input), name, vec![], vec![])
  })(input.clone())
}

/// Parses inner part of a `-define(IDENT(ARGS), BODY).` or without args `-define(IDENT, BODY).`
/// will consume end delimiter `").\n"`
fn define_with_args_body_and_terminator(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    tuple((
      // Macro name
      tok_macro_ident,
      // Optional (ARG1, ARG2, ...) with trailing comma
      opt(delimited(tok_par_open, comma_sep_macro_idents, tok_par_close)),
      tok_comma,
      // Followed by a body
      many_till(any_token, parenthesis_period_newline),
    )),
    |(ident, args, _comma, (body, _term))| {
      let args1 = args.unwrap_or_default();
      // input.parser_scope.define(&ident, &args1, &body);
      // println!("New scope {:?}", &input);
      PreprocessorNodeImpl::new_define(SourceLoc::new(&input), ident, args1, body)
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
pub(crate) fn define_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  preceded(
    |i1| dash_atom(i1, "define"),
    alt((
      context(
        "-define directive with no args and no body",
        delimited(tok_par_open, define_no_args_no_body, parenthesis_period_newline),
      ),
      // `define_with_args_body_and_terminator` will consume end delimiter
      context(
        "-define directive with optional args and body",
        preceded(tok_par_open, define_with_args_body_and_terminator),
      ),
    )),
  )(input)
}

/// Parse a `-undef(IDENT)`
pub(crate) fn undef_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      |i1| dash_atom(i1, "undef"),
      delimited(tok_par_open, tok_macro_ident, tok_par_close),
      period_eol,
    ),
    |ident: String| PreprocessorNodeImpl::new_undef(SourceLoc::new(&input), ident),
  )(input.clone())
}

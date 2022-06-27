//! Code for processing a line of tokens and pasting macro values instead of macro invocations.

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::parse_pp::parse_macro_invocation_args;
use crate::erl_syntax::preprocessor::pp_define::PreprocessorDefine;
use crate::erl_syntax::token_stream::token::Token;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::project::module::preprocess::pp_state::PreprocessState;
use crate::project::module::preprocess::pp_tok_stream::TokenStream;
use libironclad_util::mfarity::MFArity;
use nom::Finish;

fn has_any_macro_invocations(line: &[Token]) -> bool {
  line.iter().any(|t| t.is_macro_invocation())
}

fn paste_tokens(output: &mut Vec<Token>, pdef: &PreprocessorDefine, args: &[AstNode]) {
  output.extend(pdef.tokens.iter().cloned());
}

/// Given an input line of tokens, replace macro invocations with their actual body content.
/// Also substitute the macro variables.
/// Returns a wrapper struct with either original or substituted tokens.
pub(crate) fn substitute_macro_invocations<'a>(
  original_input: &str,
  tokens: &'a [Token],
  state: &mut PreprocessState<'a>,
) -> TokenStream<'a> {
  if !has_any_macro_invocations(tokens) {
    // no changes, no macro invocations
    return TokenStream::new_borrowed(tokens);
  }

  let mut substituted = Vec::with_capacity(tokens.len());
  let mut tokens_itr = tokens.iter().enumerate();

  while let Some((index, t)) = tokens_itr.next() {
    if let TokenType::MacroInvocation(macro_name) = &t.content {
      println!("Found invocation of {}", &macro_name);

      let (tail, args) = parse_as_invocation_params(original_input, &tokens[index + 1..]);

      // continue iterating the tail instead of the original input tokens
      tokens_itr = tail.tokens.iter().enumerate();

      let key = MFArity::new_local(&macro_name, args.len());

      if let Some(pdef) = state.module.root_scope.defines.get(&key) {
        paste_tokens(&mut substituted, &pdef, &args);
      } else {
        unimplemented!("error macro not defined")
      }
    } else {
      substituted.push(t.clone());
    }
  }

  TokenStream::new_owned(substituted)
}

/// Invoke parser producing a list of expressions? separated by commas
fn parse_as_invocation_params<'a>(
  original_input: &str,
  tokens: &'a [Token],
) -> (ParserInput<'a>, Vec<AstNode>) {
  let parser_input = ParserInput::new_slice(tokens);
  let (tail, ppnode) = panicking_parser_error_reporter(
    original_input,
    parser_input.clone(),
    parse_macro_invocation_args(parser_input).finish(),
    false,
  );
  (tail, ppnode)
}

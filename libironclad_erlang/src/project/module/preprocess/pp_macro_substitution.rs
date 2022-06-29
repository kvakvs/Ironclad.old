//! Code for processing a line of tokens and pasting macro values instead of macro invocations.

use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::parse_pp::parse_macro_invocation_args;
use crate::erl_syntax::preprocessor::pp_define::PreprocessorDefine;
use crate::erl_syntax::token_stream::token::Token;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::exit_codes::erl_fatal_error;
use crate::project::module::preprocess::pp_state::PreprocessState;
use crate::project::module::preprocess::pp_tok_stream::TokenStream;
use crate::source_loc::SourceLoc;
use ::function_name::named;
use libironclad_util::mfarity::MFArity;
use nom::Finish;

fn has_any_macro_invocations(line: &[Token]) -> bool {
  line.iter().any(|t| t.is_macro_invocation())
}

/// For all tokens in `pdef.tokens` paste them into the `output`.
/// If a token is a `Variable(s)` token, then try look up its name in the macro args list, and if
/// found, paste the value from `args[]` into the output.
fn paste_tokens(output: &mut Vec<Token>, pdef: &PreprocessorDefine, args: &[Vec<Token>]) {
  for t in pdef.tokens.iter() {
    match &t.content {
      TokenType::Variable(var) => {
        if let Some(arg_index) = pdef.args.iter().position(|arg_name| arg_name == var) {
          let arg = &args[arg_index];
          // TODO: Macro invocation inside a macro body
          output.extend(arg.iter().cloned());
        } else {
          output.push(t.clone());
        }
      }
      _other => output.push(t.clone()),
    }
  }
}

/// Given an input line of tokens, replace macro invocations with their actual body content.
/// Also substitute the macro variables.
/// Returns a wrapper struct with either original or substituted tokens.
#[named]
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
      // println!("Found invocation of {}", &macro_name);

      let (tail, args) = parse_as_invocation_params(original_input, &tokens[index + 1..]);

      // continue iterating the tail instead of the original input tokens
      tokens_itr = tail.tokens.iter().enumerate();

      let key = MFArity::new_local(&macro_name, args.len());

      if let Some(pdef) = state.module.root_scope.defines.get(&key) {
        paste_tokens(&mut substituted, &pdef, &args);
      } else {
        erl_fatal_error(ErlError::preprocessor_error(
          SourceLoc::unimplemented(file!(), function_name!()),
          format!("Invocation of an undefined macro: {}", macro_name),
        ));
      }
    } else {
      substituted.push(t.clone());
    }
  }

  TokenStream::new_owned(substituted)
}

/// Invoke parser producing a list of expressions? separated by commas
/// Return value: Corressponding tokens, not the AST expressions! One vec of tokens per macro arg.
fn parse_as_invocation_params<'a>(
  original_input: &str,
  tokens: &'a [Token],
) -> (ParserInput<'a>, Vec<Vec<Token>>) {
  let parser_input = ParserInput::new_slice(tokens);
  let (tail, nodes_as_tokens) = panicking_parser_error_reporter(
    original_input,
    parser_input.clone(),
    parse_macro_invocation_args(parser_input).finish(),
    false,
  );
  (tail, nodes_as_tokens)
}

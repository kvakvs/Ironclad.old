//! Code for processing a line of tokens and pasting macro values instead of macro invocations.

use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::token_stream::token::Token;
use crate::erl_syntax::parsers::token_stream::token_type::TokenType;
use crate::erl_syntax::preprocessor::parsers::parse_pp::parse_macro_invocation_args;
use crate::erl_syntax::preprocessor::pp_define::PreprocessorDefine;
use crate::exit_codes::erl_fatal_error;
use crate::project::module::module_impl::ErlModule;
use crate::project::module::preprocess::pp_state::PreprocessState;
use crate::project::module::preprocess::pp_tok_stream::TokenStream;
use crate::source_loc::SourceLoc;
use ::function_name::named;
use libironclad_util::mfarity::MFArity;
use nom::combinator::recognize;
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

  let mut output = Vec::with_capacity(tokens.len());
  let mut index = 0usize;
  let max_index = tokens.len();

  while index < max_index {
    let t = &tokens[index];

    if let TokenType::MacroInvocation(macro_name) = &t.content {
      // Parse once to get the actual arguments grouped by the commas
      // and parse twice to get the actual span of tokens affected
      let (args, args_span) =
        parse_as_invocation_params(original_input, state.module.clone(), &tokens[index + 1..]);

      // Look up the macro definition
      let key = MFArity::new_local(&macro_name, args.len());

      if let Some(pdef) = state.module.root_scope.defines.get(&key) {
        // Insert macro body and replace any macro variables with content
        paste_tokens(&mut output, &pdef, &args);
      } else {
        erl_fatal_error(ErlError::preprocessor_error(
          SourceLoc::unimplemented(file!(), function_name!()),
          format!("Invocation of an undefined macro: {}", macro_name),
        ));
      }

      // Skip input tokens consumed by parsing the arguments
      index += args_span + 1;
    } else {
      output.push(t.clone());
      index += 1;
    }
  }

  TokenStream::new_owned(output)
}

/// Invoke parser producing a list of expressions? separated by commas
/// Return value: The tokens of arguments, grouped by the separating commas, and the span of the
///               arguments list (used to skip the length of tokens)
fn parse_as_invocation_params(
  original_input: &str,
  module: ErlModule,
  tokens: &[Token],
) -> (Vec<Vec<Token>>, usize) {
  let parser_input = ParserInput::new_slice(module.clone(), tokens);
  let (_tail, nodes_as_tokens) = panicking_parser_error_reporter(
    original_input,
    parser_input.clone(),
    parse_macro_invocation_args(parser_input).finish(),
    false,
  );

  // Assume this cannot fail, if previous failed it should panic or something
  let parser_input2 = ParserInput::new_slice(module, tokens);
  let (_tail2, recognized_span) = recognize(parse_macro_invocation_args)(parser_input2)
    .finish()
    .unwrap();

  (nodes_as_tokens, recognized_span.tokens.len())
}

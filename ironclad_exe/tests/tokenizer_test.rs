mod test_util;

use ::function_name::named;
use libironclad_erlang::erl_syntax::parsers::misc::panicking_tokenizer_error_reporter;
use libironclad_erlang::erl_syntax::token_stream::tokenizer::tokenize_source;
use nom::Finish;

#[test]
#[named]
fn test_tok_macro_invocation1() {
  test_util::start(function_name!(), "Tokenize a macro invocation");

  let input = "
	    ?NO_DEBUG('code disassembling failed: ~p~n', [Rsn]),
	    ";
  let tokens = panicking_tokenizer_error_reporter(input, tokenize_source(input).finish());
  tokens.into_iter().for_each(|t| print!("{} ", t));
}

#[test]
#[named]
fn test_tok_comment() {
  test_util::start(function_name!(), "Tokenize a line comment");

  let input = "
	    %% =====================================================================
%% is_pure(Module, Name, Arity) -> boolean()
	    ";
  let tokens = panicking_tokenizer_error_reporter(input, tokenize_source(input).finish());
  tokens.into_iter().for_each(|t| print!("{} ", t));
}

mod test_util;

use ::function_name::named;
use libironclad_erlang::erl_syntax::parsers::misc::panicking_tokenizer_error_reporter;
use libironclad_erlang::erl_syntax::parsers::token_stream::token_type::TokenType;
use libironclad_erlang::erl_syntax::parsers::token_stream::tokenizer::tokenize_source;
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

#[test]
#[named]
fn test_tok_integer() {
  test_util::start(function_name!(), "Tokenize integers");

  let input = "16#8000, -16#8000";
  let tokens = panicking_tokenizer_error_reporter(input, tokenize_source(input).finish());
  assert_eq!(tokens.len(), 3);

  if let TokenType::Integer(i) = &tokens[0].content {
    assert_eq!(i.as_usize(), Some(0x8000));
  } else {
    panic!("Bad token 0, expected integer")
  }
  if let TokenType::Integer(j) = &tokens[2].content {
    assert_eq!(j.as_isize(), Some(-0x8000));
  } else {
    panic!("Bad token 2, expected integer")
  }
  tokens.into_iter().for_each(|t| print!("{} ", t));
}

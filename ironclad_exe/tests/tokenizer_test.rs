mod test_util;

use ::function_name::named;
use libironclad_erlang::erl_syntax::parsers::misc::panicking_tokenizer_error_reporter;
use libironclad_erlang::erl_syntax::parsers::token_stream::token::format_tok_stream;
use libironclad_erlang::erl_syntax::parsers::token_stream::token_kind::TokenKind;
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

  assert_eq!(tokens.len(), 4);
  println!("TOKENS: {}", format_tok_stream(&tokens, tokens.len()));

  if let TokenKind::Integer(i) = &tokens[0].kind {
    assert_eq!(i.as_usize(), Some(0x8000));
  } else {
    panic!("Bad token 0, expected integer")
  }

  assert!(tokens[2].kind.is_same_type(&TokenKind::Minus), "Bad token 2 expected Minus");

  if let TokenKind::Integer(j) = &tokens[3].kind {
    assert_eq!(
      j.as_isize(),
      Some(0x8000),
      "Expected 0x8000, minus is merged in later when parsing unary ops"
    );
  } else {
    panic!("Bad token 3, expected integer")
  }
  tokens.into_iter().for_each(|t| print!("{} ", t));
}

#[named]
#[test]
fn tok_dollar_char_test() {
  test_util::start(function_name!(), "Tokenize $characters");
  {
    let input1 = "$\\n";
    let tok1 = test_util::tokenize(input1);
    assert!(
      tok1[0].is_escaped_char_of('n'),
      "Expected $\\n to tokenize to EscapedChar('n') but received {:?}",
      tok1[0]
    );
  }
  {
    let input2 = "$|";
    let tok2 = test_util::tokenize(input2);
    assert!(
      tok2[0].is_char_of('|'),
      "Expected $| to tokenize to Char(|) but received {:?}",
      tok2[0]
    );
  }
}

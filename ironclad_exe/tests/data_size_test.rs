extern crate function_name;
extern crate libironclad_erlang;

mod test_util;

use ::function_name::named;
use libironclad_erlang::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use libironclad_erlang::erl_syntax::erl_ast::AstNode;
use libironclad_erlang::erl_syntax::parsers::token_stream::token::Token;
use libironclad_erlang::erl_syntax::parsers::token_stream::token_kind::TokenKind;

#[named]
#[test]
fn ast_size_test() {
  test_util::start(function_name!(), "AST size test");
  const TOKEN: usize = std::mem::size_of::<Token>();
  println!("sizeof Token={}", TOKEN);

  const T_TYPE: usize = std::mem::size_of::<TokenKind>();
  println!("sizeof TokenType={}", T_TYPE);

  const AST_NODE: usize = std::mem::size_of::<AstNode>();
  println!("sizeof AstNode={}", AST_NODE);

  const AST_NODE_IMPL: usize = std::mem::size_of::<AstNodeImpl>();
  println!("sizeof AstNodeImpl={}", AST_NODE_IMPL);

  const AST_NODE_TYPE: usize = std::mem::size_of::<AstNodeType>();
  println!("sizeof AstNodeType={}", AST_NODE_TYPE);

  assert!(TOKEN <= 56, "Token size must not grow unchecked");
  assert!(T_TYPE <= 48, "TokenType size must not grow unchecked");
  assert!(AST_NODE <= 8, "AstNode size must not grow unchecked");
  assert!(AST_NODE_IMPL <= 120, "AstNodeImpl size must not grow unchecked");
  assert!(AST_NODE_TYPE <= 104, "AstNodeType size must not grow unchecked");
}

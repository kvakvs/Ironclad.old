//! Input is broken into tokens

use crate::colored::Colorize;
use crate::erl_syntax::parsers::token_stream::keyword::Keyword;
use crate::erl_syntax::parsers::token_stream::token_type::TokenType;
use crate::typing::erl_integer::ErlInteger;
use std::ptr::null;

/// Token represents basic elements of source code
#[derive(Clone)]
pub struct Token {
  /// Pointer to source
  pub offset: *const u8,
  /// The token itself
  pub content: TokenType,
}

impl Token {
  /// Create a new keyword token
  #[inline]
  pub fn new_keyword(offset: *const u8, k: Keyword) -> Self {
    Self {
      offset,
      content: TokenType::Keyword(k),
      // last_in_line: false,
    }
  }

  /// Create a new symbol token
  #[inline]
  pub fn new(offset: *const u8, tt: TokenType) -> Self {
    Self { offset, content: tt }
  }

  /// Create a new End of Line
  #[inline]
  pub fn new_eol() -> Self {
    Self { offset: null(), content: TokenType::EOL }
  }

  /// Create a new token for small integer
  #[inline]
  pub fn new_small(i: i64) -> Self {
    Self {
      offset: null(),
      content: TokenType::Integer(ErlInteger::Small(i)),
    }
  }

  /// Create a new token for string
  #[inline]
  pub fn new_string(s: String) -> Self {
    Self { offset: null(), content: TokenType::Str(s.into()) }
  }

  /// Create a new token for atom
  #[inline]
  pub fn new_atom(s: String) -> Self {
    Self { offset: null(), content: TokenType::Atom(s) }
  }

  /// Check whether the token is a newline token
  #[inline]
  pub fn is_eol(&self) -> bool {
    matches!(self.content, TokenType::EOL)
  }

  /// Check whether the token is an atom of given value
  #[inline]
  pub fn is_atom_of(&self, sample: &str) -> bool {
    match &self.content {
      TokenType::Atom(s) => s == sample,
      _ => false,
    }
  }
  /// Check whether the token is an atom
  #[inline]
  pub fn is_atom(&self) -> bool {
    matches!(&self.content, TokenType::Atom(_))
  }

  /// Check whether the token is a keyword of given value
  #[inline]
  pub fn is_keyword(&self, sample: Keyword) -> bool {
    match &self.content {
      TokenType::Keyword(kw) => kw == &sample,
      _ => false,
    }
  }

  /// Check whether the token is a character
  #[inline]
  pub fn is_char_of(&self, ch: char) -> bool {
    match &self.content {
      TokenType::Character(c) => *c == ch,
      _ => false,
    }
  }

  /// Check whether the token is an escaped character
  #[inline]
  pub fn is_escaped_char_of(&self, ch: char) -> bool {
    match &self.content {
      TokenType::EscapedCharacter { in_source, .. } => *in_source == ch,
      _ => false,
    }
  }

  /// Check whether the token is a macro invocation
  #[inline]
  pub fn is_macro_invocation(&self) -> bool {
    matches!(self.content, TokenType::MacroInvocation(_))
  }

  /// Check whether the token is a given type token
  #[inline]
  pub fn is_tok(&self, tt: TokenType) -> bool {
    self.content.is_same_type(&tt)
  }

  /// Check whether the token array ends with given token types
  pub fn ends_with(tokens: &[Token], ttypes: &[TokenType]) -> bool {
    if ttypes.len() > tokens.len() {
      return false;
    }
    let mut it_tokens = tokens.iter().rev();
    let mut it_ttypes = ttypes.iter().rev();
    loop {
      match (it_tokens.next(), it_ttypes.next()) {
        (Some(to), Some(ty)) => {
          // If enum type doesn't match, return false
          if std::mem::discriminant(&to.content) != std::mem::discriminant(ty) {
            return false;
          }
        }
        // End of types, but not end of input
        (_, None) => return true,
        // End of input but not end of types (this is also an error)
        (None, _) => unreachable!(),
      }
    }
  }
}

impl std::fmt::Debug for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Token[{:?}]", self.content)
  }
}

impl std::fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.content.fmt(f)
  }
}

/// A temporary solution for displaying token streams without a pile of commas between each token
pub fn format_tok_stream(tokens: &[Token], cut: usize) -> String {
  let out: String = tokens.iter().take(cut).map(|t| format!(" {}", t)).collect();
  out.bright_yellow().on_blue().to_string()
}

/// A temporary solution for displaying token streams without a pile of commas between each token.
/// Stops at newline or stream end.
pub fn format_tok_till_eol(tokens: &[Token]) -> String {
  let out: String = tokens
    .iter()
    .take_while(|&t| !t.is_eol())
    .map(|t| format!(" {}", t))
    .collect();
  out.bright_yellow().on_blue().to_string()
}

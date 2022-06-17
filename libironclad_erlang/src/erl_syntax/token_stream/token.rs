//! Input is broken into tokens

use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::token_type::TokenType;
use libironclad_util::pretty::Pretty;

/// Token represents basic elements of source code
#[derive(Clone)]
pub struct Token {
  /// Pointer to source
  pub offset: *const u8,
  /// The token itself
  pub content: TokenType,
  // /// True for the last item in line before `\n`. Field updated during preprocessing.
  // pub last_in_line: bool,
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
    Self {
      offset,
      content: tt,
      //last_in_line: false
    }
  }

  /// Check whether the token is a newline token (temporary till preprocessing stage)
  #[inline]
  pub fn is_newline(&self) -> bool {
    matches!(self.content, TokenType::Newline)
  }

  /// Check whether the token is an atom of given value
  #[inline]
  pub fn is_atom_of(&self, sample: &str) -> bool {
    match &self.content {
      TokenType::Atom(s) => s == sample,
      _ => false,
    }
  }

  /// Check whether the token is a keyword of given value
  #[inline]
  pub fn is_keyword(&self, sample: Keyword) -> bool {
    match &self.content {
      TokenType::Keyword(kw) => kw == &sample,
      _ => false,
    }
  }
}

impl std::fmt::Debug for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self)
  }
}

impl std::fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.content {
      TokenType::Atom(a) => Pretty::singlequot_string(f, a),
      TokenType::Bar => write!(f, "|"),
      TokenType::BarBar => write!(f, "||"),
      TokenType::Colon => write!(f, ":"),
      TokenType::ColonColon => write!(f, "::"),
      TokenType::Comma => write!(f, ","),
      TokenType::CurlyClose => write!(f, "}}"),
      TokenType::CurlyOpen => write!(f, "{{"),
      TokenType::Div => write!(f, "/"),
      TokenType::DoubleAngleClose => write!(f, ">>"),
      TokenType::DoubleAngleOpen => write!(f, "<<"),
      TokenType::EqualEqual => write!(f, "=="),
      TokenType::EqualSymbol => write!(f, "="),
      TokenType::GreaterEq => write!(f, ">="),
      TokenType::GreaterThan => write!(f, ">"),
      TokenType::HardEq => write!(f, "=:="),
      TokenType::HardNotEq => write!(f, "=/="),
      TokenType::Hash => write!(f, "#"),
      TokenType::Integer(i) => write!(f, "{}", i),
      TokenType::Float(flt) => write!(f, "{}", flt),
      TokenType::Keyword(kw) => write!(f, "{}", kw),
      TokenType::LeftArr => write!(f, "<-"),
      TokenType::LeftDoubleArr => write!(f, "<="),
      TokenType::LessThan => write!(f, "<"),
      TokenType::LessThanEq => write!(f, "=<"),
      TokenType::ListAppend => write!(f, "++"),
      TokenType::ListSubtract => write!(f, "--"),
      TokenType::Minus => write!(f, "-"),
      TokenType::Mul => write!(f, "*"),
      TokenType::NotEq => write!(f, "/="),
      TokenType::ParClose => write!(f, ")"),
      TokenType::ParOpen => write!(f, "("),
      TokenType::Period => write!(f, "."),
      TokenType::PeriodPeriod => write!(f, ".."),
      TokenType::Plus => write!(f, "+"),
      TokenType::RightArr => write!(f, "->"),
      TokenType::RightDoubleArr => write!(f, "=>"),
      TokenType::Semicolon => write!(f, ";"),
      TokenType::Send => write!(f, "!"),
      TokenType::SquareClose => write!(f, "]"),
      TokenType::SquareOpen => write!(f, "["),
      TokenType::Str(s) => Pretty::doublequot_string(f, s),
      TokenType::Variable(v) => write!(f, "{}", v),
      TokenType::MacroInvocation(m) => write!(f, "?{}", m),
      TokenType::Character(c) => write!(f, "${}", *c),
      TokenType::Comment(c) => write!(f, "% {}", c),
      TokenType::Newline => writeln!(f, ""),
    }
  }
}

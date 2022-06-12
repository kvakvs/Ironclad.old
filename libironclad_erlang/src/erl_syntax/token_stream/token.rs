//! Input is broken into tokens

use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::tok_strings::Char;
use crate::typing::erl_integer::ErlInteger;
use libironclad_util::pretty::Pretty;
use std::fmt::Formatter;

/// Temporary token_stream marking tokens of interest while parsing the AST tree. Must not be present in
/// the final AST produced by the parser.
#[allow(missing_docs)]
#[derive(Debug, Eq, PartialEq)]
pub enum Token {
  Comma,
  Semicolon,
  Colon,
  Period,
  Plus,
  Minus,
  /// Float division `/`
  Div,
  /// Multiplication `*`
  Mul,
  /// `++` append list
  ListAppend,
  /// `--` set operation on list `A -- B`
  ListSubtract,
  EqualEqual,
  NotEq,
  LessThanEq,
  LessThan,
  GreaterEq,
  GreaterThan,
  /// `==` exact equality with type
  HardEq,
  /// `=/=` exact inequality with type
  HardNotEq,
  /// `=` equals symbol
  EqualSymbol,
  /// `=>` double right arrow
  RightDoubleArr,
  /// `->` single right arrow
  RightArr,
  /// `<==` double left arrow
  LeftDoubleArr,
  /// `<-` single left arrow
  LeftArr,
  /// A send operation `!`
  Send,
  ParOpen,
  ParClose,
  SquareOpen,
  SquareClose,
  CurlyOpen,
  CurlyClose,
  /// `<<` opening a binary
  DoubleAngleOpen,
  /// `>>` closing a binary
  DoubleAngleClose,
  Hash,
  Bar,
  BarBar,
  /// A parsed string token_stream between `" TEXT "`
  Str(String),
  Comment(String),
  /// A `$`-prefixed any character
  Character(Char),
  /// A parsed atom token_stream either lowercase `atom` or quoted between `' TEXT '`
  Atom(String),
  Variable(String),
  Keyword(Keyword),
  Integer(ErlInteger),
  MacroInvocation(String),
}

impl std::fmt::Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Token::Atom(a) => Pretty::singlequot_string(f, a),
      Token::Bar => write!(f, "|"),
      Token::BarBar => write!(f, "||"),
      Token::Colon => write!(f, ":"),
      Token::Comma => write!(f, ","),
      Token::CurlyClose => write!(f, "}}"),
      Token::CurlyOpen => write!(f, "{{"),
      Token::Div => write!(f, "/"),
      Token::DoubleAngleClose => write!(f, ">>"),
      Token::DoubleAngleOpen => write!(f, "<<"),
      Token::EqualEqual => write!(f, "=="),
      Token::EqualSymbol => write!(f, "="),
      Token::GreaterEq => write!(f, ">="),
      Token::GreaterThan => write!(f, ">"),
      Token::HardEq => write!(f, "=:="),
      Token::HardNotEq => write!(f, "=/="),
      Token::Hash => write!(f, "#"),
      Token::Integer(i) => write!(f, "{}", i),
      Token::Keyword(kw) => write!(f, "{}", kw),
      Token::LeftArr => write!(f, "<-"),
      Token::LeftDoubleArr => write!(f, "<="),
      Token::LessThan => write!(f, "<"),
      Token::LessThanEq => write!(f, "=<"),
      Token::ListAppend => write!(f, "++"),
      Token::ListSubtract => write!(f, "--"),
      Token::Minus => write!(f, "-"),
      Token::Mul => write!(f, "*"),
      Token::NotEq => write!(f, "/="),
      Token::ParClose => write!(f, ")"),
      Token::ParOpen => write!(f, "("),
      Token::Period => write!(f, "."),
      Token::Plus => write!(f, "+"),
      Token::RightArr => write!(f, "->"),
      Token::RightDoubleArr => write!(f, "=>"),
      Token::Semicolon => write!(f, ";"),
      Token::Send => write!(f, "!"),
      Token::SquareClose => write!(f, "]"),
      Token::SquareOpen => write!(f, "["),
      Token::Str(s) => Pretty::doublequot_string(f, s),
      Token::Variable(v) => write!(f, "{}", v),
      Token::MacroInvocation(m) => write!(f, "?{}", m),
      Token::Character(c) => write!(f, "${}", *c),
      Token::Comment(c) => write!(f, "% {}", c),
    }
  }
}

//! Type tags for tokens

use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::tok_strings::Char;
use crate::typing::erl_integer::ErlInteger;
use std::sync::Arc;

/// Temporary token_stream marking tokens of interest while parsing the AST tree. Must not be present in
/// the final AST produced by the parser.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
  Comma,
  Semicolon,
  Colon,
  ColonColon,
  Period,
  PeriodPeriod,
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
  Str(Arc<String>),
  Comment(Arc<String>),
  /// A `$`-prefixed any character
  Character(Char),
  /// A parsed atom token_stream either lowercase `atom` or quoted between `' TEXT '`
  Atom(String),
  Variable(String),
  Keyword(Keyword),
  Integer(ErlInteger),
  Float(f64),
  MacroInvocation(String),
}

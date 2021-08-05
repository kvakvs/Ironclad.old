//! Declares temporary tokens for the AST

use std::fmt::Formatter;

/// Temporary token marking tokens of interest while parsing the AST tree. Must not be present in
/// the final AST produced by the parser.
#[allow(missing_docs)]
#[derive(Eq, PartialEq)]
pub enum ErlToken {
  Comma,
  Plus,
  Minus,
  Div,
  Mul,
  IntegerDiv,
  Remainder,
  Not,
  Or,
  Xor,
  And,
  BinaryNot,
  BinaryAnd,
  BinaryOr,
  BinaryXor,
  BinaryShiftLeft,
  BinaryShiftRight,
  ListAppend,
  ListSubtract,
  Eq,
  NotEq,
  LessThanEq,
  LessThan,
  GreaterEq,
  GreaterThan,
  HardEq,
  HardNotEq,
  AndAlso,
  OrElse,
  Assign,
  Send,
  Catch,
}

impl std::fmt::Display for ErlToken {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      ErlToken::Comma => write!(f, ","),
      ErlToken::Plus => write!(f, "+"),
      ErlToken::Minus => write!(f, "-"),
      ErlToken::Div => write!(f, "/"),
      ErlToken::Mul => write!(f, "*"),
      ErlToken::IntegerDiv => write!(f, "div"),
      ErlToken::Remainder => write!(f, "rem"),
      ErlToken::Not => write!(f, "not"),
      ErlToken::Or => write!(f, "or"),
      ErlToken::Xor => write!(f, "xor"),
      ErlToken::And => write!(f, "and"),
      ErlToken::BinaryNot => write!(f, "bnot"),
      ErlToken::BinaryAnd => write!(f, "band"),
      ErlToken::BinaryOr => write!(f, "bor"),
      ErlToken::BinaryXor => write!(f, "bxor"),
      ErlToken::BinaryShiftLeft => write!(f, "bsl"),
      ErlToken::BinaryShiftRight => write!(f, "bsr"),
      ErlToken::ListAppend => write!(f, "++"),
      ErlToken::ListSubtract => write!(f, "--"),
      ErlToken::Eq => write!(f, "=="),
      ErlToken::NotEq => write!(f, "/="),
      ErlToken::LessThanEq => write!(f, "=<"),
      ErlToken::LessThan => write!(f, "<"),
      ErlToken::GreaterEq => write!(f, ">="),
      ErlToken::GreaterThan => write!(f, ">"),
      ErlToken::HardEq => write!(f, "=:="),
      ErlToken::HardNotEq => write!(f, "=/="),
      ErlToken::AndAlso => write!(f, "andalso"),
      ErlToken::OrElse => write!(f, "orelse"),
      ErlToken::Assign => write!(f, "="),
      ErlToken::Send => write!(f, "!"),
      ErlToken::Catch => write!(f, "catch"),
    }
  }
}
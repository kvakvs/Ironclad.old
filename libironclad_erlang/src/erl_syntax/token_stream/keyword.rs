//! Keyword Keywords enum

/// Recognized keywords
#[allow(missing_docs)]
#[derive(Debug, Eq, PartialEq)]
pub enum Keyword {
  After,
  And,
  AndAlso,
  Begin,
  BinaryAnd,
  BinaryNot,
  BinaryOr,
  BinaryShiftLeft,
  BinaryShiftRight,
  BinaryXor,
  Case,
  Catch,
  Cond, // not in the language?
  End,
  Fun,
  If,
  Let, // not in the language?
  IntegerDiv,
  Maybe,
  Not,
  Of,
  Or,
  OrElse,
  Receive,
  Rem,
  Try,
  When,
  Xor,
}

impl std::fmt::Display for Keyword {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Keyword::IntegerDiv => write!(f, "div"),
      Keyword::Rem => write!(f, "rem"),
      Keyword::Not => write!(f, "not"),
      Keyword::Or => write!(f, "or"),
      Keyword::Xor => write!(f, "xor"),
      Keyword::And => write!(f, "and"),
      Keyword::BinaryNot => write!(f, "bnot"),
      Keyword::BinaryAnd => write!(f, "band"),
      Keyword::BinaryOr => write!(f, "bor"),
      Keyword::BinaryXor => write!(f, "bxor"),
      Keyword::BinaryShiftLeft => write!(f, "bsl"),
      Keyword::BinaryShiftRight => write!(f, "bsr"),
      Keyword::AndAlso => write!(f, "andalso"),
      Keyword::OrElse => write!(f, "orelse"),
      Keyword::Catch => write!(f, "catch"),
      Keyword::After => write!(f, "after"),
      Keyword::Begin => write!(f, "begin"),
      Keyword::Case => write!(f, "case"),
      Keyword::Cond => write!(f, "cond"),
      Keyword::End => write!(f, "end"),
      Keyword::Fun => write!(f, "fun"),
      Keyword::If => write!(f, "if"),
      Keyword::Let => write!(f, "let"),
      Keyword::Maybe => write!(f, "maybe"),
      Keyword::Of => write!(f, "of"),
      Keyword::Receive => write!(f, "receive"),
      Keyword::Try => write!(f, "try"),
      Keyword::When => write!(f, "when"),
    }
  }
}

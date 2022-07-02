//! Keyword Keywords enum

/// Recognized keywords
#[allow(missing_docs)]
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
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
  Else,
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

impl Keyword {
  /// Stringify the keyword
  pub fn to_str(&self) -> &'static str {
    match self {
      Keyword::IntegerDiv => "div",
      Keyword::Rem => "rem",
      Keyword::Not => "not",
      Keyword::Or => "or",
      Keyword::Xor => "xor",
      Keyword::And => "and",
      Keyword::BinaryNot => "bnot",
      Keyword::BinaryAnd => "band",
      Keyword::BinaryOr => "bor",
      Keyword::BinaryXor => "bxor",
      Keyword::BinaryShiftLeft => "bsl",
      Keyword::BinaryShiftRight => "bsr",
      Keyword::AndAlso => "andalso",
      Keyword::OrElse => "orelse",
      Keyword::Catch => "catch",
      Keyword::After => "after",
      Keyword::Begin => "begin",
      Keyword::Case => "case",
      Keyword::Cond => "cond",
      Keyword::Else => "else",
      Keyword::End => "end",
      Keyword::Fun => "fun",
      Keyword::If => "if",
      Keyword::Let => "let",
      Keyword::Maybe => "maybe",
      Keyword::Of => "of",
      Keyword::Receive => "receive",
      Keyword::Try => "try",
      Keyword::When => "when",
    }
  }
}

impl std::fmt::Display for Keyword {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.to_str().fmt(f)
  }
}

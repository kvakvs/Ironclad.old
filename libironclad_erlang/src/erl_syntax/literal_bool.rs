//! Literal boolean expressions return this value

/// Returned by walk litexpr function, for boolean expression calculations
#[derive(Debug, Eq, PartialEq)]
pub enum LiteralBool {
  /// A literal expression resolves to a false
  False,
  /// A literal expression resolves to a true
  True,
  /// A literal expression resolves to something which isn't true or false atom
  NotABoolean,
}

impl LiteralBool {
  /// Negate a LiteralBool expression
  pub(crate) fn negate(&self) -> LiteralBool {
    match self {
      LiteralBool::False => LiteralBool::True,
      LiteralBool::True => LiteralBool::False,
      LiteralBool::NotABoolean => LiteralBool::NotABoolean,
    }
  }

  /// Calculate a conjunction of a LiteralBool expression with another (AND operation)
  /// `False` with any value even a non-boolean will return `False`.
  pub(crate) fn and(&self, other: &LiteralBool) -> LiteralBool {
    match (self, other) {
      (_, LiteralBool::False) => LiteralBool::False,
      (LiteralBool::False, _) => LiteralBool::False,
      (LiteralBool::True, LiteralBool::True) => LiteralBool::True,
      _ => LiteralBool::NotABoolean,
    }
  }

  /// Calculate a disjunction of a LiteralBool expression with another (OR operation)
  /// `True` with any value even a non-boolean will return `True`.
  pub(crate) fn or(&self, other: &LiteralBool) -> LiteralBool {
    match (self, other) {
      (LiteralBool::True, _) => LiteralBool::True,
      (_, LiteralBool::True) => LiteralBool::True,
      (LiteralBool::False, LiteralBool::False) => LiteralBool::False,
      _ => LiteralBool::NotABoolean,
    }
  }

  /// Calculate a XOR result of a LiteralBool expression with another
  pub(crate) fn xor(&self, other: &LiteralBool) -> LiteralBool {
    match (self, other) {
      (LiteralBool::NotABoolean, _) => LiteralBool::NotABoolean,
      (_, LiteralBool::NotABoolean) => LiteralBool::NotABoolean,
      (a, b) => {
        if a != b {
          LiteralBool::True
        } else {
          LiteralBool::False
        }
      }
    }
  }
}

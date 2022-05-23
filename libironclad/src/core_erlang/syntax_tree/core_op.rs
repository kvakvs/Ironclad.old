//! Binary and unary operations used in type checking.

use libironclad_erlang::syntax_tree::erl_op::ErlBinaryOp;

/// Binary operation taking two arguments
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CoreBinaryOp {
  /// Joins two expressions together, right becomes the result
  Comma,
  /// Sum of two any numbers
  Add,
  /// Subtraction resut of two any numbers
  Sub,
  /// Product of two any numbers
  Mul,
  /// Float division result of two any numbers
  Div,
  /// Integer division result of two integer numbers
  IntegerDiv,
  /// Division remainder of two integer numbers
  Modulo,
  /// Left value is less than the right value
  Less,
  /// Left value is greater than the right value
  Greater,
  /// Left value is less or equal to the right value
  LessEq,
  /// Left value is greater or equal to the right value
  GreaterEq,
  /// Equality of two any values
  Eq,
  /// Inequality of two any values
  NotEq,
  /// Hard equality (identity) of two any values
  HardEq,
  /// Hard inequality (identity mismatch) of two any values
  HardNotEq,
  /// Concatenate two lists ++
  ListAppend,
  /// Difference of two lists --
  ListSubtract,
  /// Semicolon (or) operator for guard exprs
  Semicolon,
  /// Logical and
  And,
  /// Andalso operator
  AndAlso,
  /// Logical or
  Or,
  /// Orelse operatoir
  OrElse,
  /// Logical xor
  Xor,
  /// Binary and
  BinaryAnd,
  /// Binary or
  BinaryOr,
  /// Binary xor
  BinaryXor,
  /// Bsl ironclad_exe
  BinaryShiftLeft,
  /// Bsr ironclad_exe
  BinaryShiftRight,
  /// Assignment/match = operator
  Match,
  /// Send operator "!"
  Bang,
}

impl From<ErlBinaryOp> for CoreBinaryOp {
  fn from(erlop: ErlBinaryOp) -> Self {
    match erlop {
      ErlBinaryOp::Comma => CoreBinaryOp::Comma,
      ErlBinaryOp::Add => CoreBinaryOp::Add,
      ErlBinaryOp::Sub => CoreBinaryOp::Sub,
      ErlBinaryOp::Mul => CoreBinaryOp::Mul,
      ErlBinaryOp::Div => CoreBinaryOp::Div,
      ErlBinaryOp::IntegerDiv => CoreBinaryOp::IntegerDiv,
      ErlBinaryOp::Remainder => CoreBinaryOp::Modulo,
      ErlBinaryOp::Less => CoreBinaryOp::Less,
      ErlBinaryOp::Greater => CoreBinaryOp::Greater,
      ErlBinaryOp::LessEq => CoreBinaryOp::LessEq,
      ErlBinaryOp::GreaterEq => CoreBinaryOp::GreaterEq,
      ErlBinaryOp::Eq => CoreBinaryOp::Eq,
      ErlBinaryOp::NotEq => CoreBinaryOp::NotEq,
      ErlBinaryOp::HardEq => CoreBinaryOp::HardEq,
      ErlBinaryOp::HardNotEq => CoreBinaryOp::HardNotEq,
      ErlBinaryOp::ListAppend => CoreBinaryOp::ListAppend,
      ErlBinaryOp::ListSubtract => CoreBinaryOp::ListSubtract,
      ErlBinaryOp::Semicolon => CoreBinaryOp::Semicolon,
      ErlBinaryOp::And => CoreBinaryOp::And,
      ErlBinaryOp::AndAlso => CoreBinaryOp::AndAlso,
      ErlBinaryOp::Or => CoreBinaryOp::Or,
      ErlBinaryOp::OrElse => CoreBinaryOp::OrElse,
      ErlBinaryOp::Xor => CoreBinaryOp::Xor,
      ErlBinaryOp::BinaryAnd => CoreBinaryOp::BinaryAnd,
      ErlBinaryOp::BinaryOr => CoreBinaryOp::BinaryOr,
      ErlBinaryOp::BinaryXor => CoreBinaryOp::BinaryXor,
      ErlBinaryOp::BinaryShiftLeft => CoreBinaryOp::BinaryShiftLeft,
      ErlBinaryOp::BinaryShiftRight => CoreBinaryOp::BinaryShiftRight,
      ErlBinaryOp::Match => CoreBinaryOp::Match,
      ErlBinaryOp::Bang => CoreBinaryOp::Bang,
    }
  }
}

/// Unary operation takes 1 argument of bool or number, and returns same type
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CoreUnaryOp {
  /// Prefixed by 'catch' keyword
  Catch,
  /// Logical negation
  Not,
  /// Numerical sign change, -X
  Negative,
  /// Numerical sign positive, no sign change: +X
  Positive,
}

impl std::fmt::Display for CoreBinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      CoreBinaryOp::Add => write!(f, "+"),
      CoreBinaryOp::Sub => write!(f, "-"),
      CoreBinaryOp::Mul => write!(f, "*"),
      CoreBinaryOp::Div => write!(f, "/"),
      CoreBinaryOp::IntegerDiv => write!(f, "div"),
      CoreBinaryOp::Modulo => write!(f, "mod"),
      CoreBinaryOp::Less => write!(f, "<"),
      CoreBinaryOp::Greater => write!(f, ">"),
      CoreBinaryOp::LessEq => write!(f, "⩽"),
      CoreBinaryOp::GreaterEq => write!(f, "⩾"),
      CoreBinaryOp::Eq => write!(f, "≃"),
      CoreBinaryOp::NotEq => write!(f, "≄"),
      CoreBinaryOp::HardEq => write!(f, "≡"),
      CoreBinaryOp::HardNotEq => write!(f, "≢"),
      CoreBinaryOp::ListAppend => write!(f, "++"),
      CoreBinaryOp::ListSubtract => write!(f, "--"),
      CoreBinaryOp::Comma => write!(f, ","),
      CoreBinaryOp::Semicolon => write!(f, ";"),
      CoreBinaryOp::And => write!(f, "and"),
      CoreBinaryOp::AndAlso => write!(f, "andalso"),
      CoreBinaryOp::Or => write!(f, "or"),
      CoreBinaryOp::OrElse => write!(f, "orelse"),
      CoreBinaryOp::Xor => write!(f, "xor"),
      CoreBinaryOp::BinaryAnd => write!(f, "band"),
      CoreBinaryOp::BinaryOr => write!(f, "bor"),
      CoreBinaryOp::BinaryXor => write!(f, "bxor"),
      CoreBinaryOp::BinaryShiftLeft => write!(f, "bsl"),
      CoreBinaryOp::BinaryShiftRight => write!(f, "bsr"),
      CoreBinaryOp::Match => write!(f, "="),
      CoreBinaryOp::Bang => write!(f, "!"),
    }
  }
}

impl std::fmt::Display for CoreUnaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      CoreUnaryOp::Not => write!(f, "not"),
      CoreUnaryOp::Negative => write!(f, "-"),
      CoreUnaryOp::Positive => write!(f, "+"),
      CoreUnaryOp::Catch => write!(f, "catch"),
    }
  }
}
